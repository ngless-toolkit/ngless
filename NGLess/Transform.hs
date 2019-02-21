{- Copyright 2016-2019 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}

module Transform
    ( transform
    , pureTransform
    , isVarUsed
    , isVarUsed1
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Cont
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Arrow (first, second)
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.Hash.MD5 as MD5
import qualified Data.Map.Strict as M
import           Data.List (sortOn, foldl')

import Language
import Modules
import Output
import NGLess
import Utils.Utils (uniq, secondM)
import NGLess.NGLEnvironment
import BuiltinFunctions


{-| Before interpretation, scripts are transformed to allow for several
  - optimizations.

  - As a first step, the script is normalized, introducing temporary variables
  - so that function calls do not contain nested expressions.  For example:
  -
  -     write(mapstats(samfile('input.sam')), ofile='stats.txt')
  -
  - is re-written to the equivalent of:
  -
  -     temp$0 = samfile('input.sam')
  -     temp$1 = mapstats(temp$0)
  -     write(temp$1, ofile='stats.txt')
  -
  - Note that "temp$xx" are not valid ngless variable names. Thus, these
  - temporary variables can only be introduced internally and will never clash
  - with any user variables.
-}
transform :: [Module] -> Script -> NGLessIO Script
transform mods sc = Script (nglHeader sc) <$> applyM transforms (nglBody sc)
    where
        applyM [] e = return e
        applyM (t:ts) e = t e >>= applyM ts
        transforms = preTransforms ++ modTransforms ++ builtinTransforms
        modTransforms = map modTransform mods
        preTransforms =
                [ reassignPreprocess
                , addTemporaries
                , addOutputHash -- Hashing should be based on what the user input (pre-transforms)
                ]
        builtinTransforms =
                [ writeToMove
                , qcInPreprocess
                , ifLenDiscardSpecial
                , substrimReassign
                , addFileChecks
                , addIndexChecks
                ]

pureRecursiveTransform :: (Expression -> Expression) -> Expression -> Expression
pureRecursiveTransform f e = runIdentity (recursiveTransform (return . f) e)

-- | A little helper function which turns a lifts a pure transform `Expression
-- -> Expression` into the generic `[(Int, Expression)] -> NGLessIO [(Int, Expression)]`
pureTransform :: (Expression -> Expression) -> [(Int,Expression)] -> NGLessIO [(Int, Expression)]
pureTransform f = return . map (second (pureRecursiveTransform f))

addArgument :: T.Text -- ^ function name
            -> (Variable, Expression) -- ^ new argument
            -> Expression -- ^ expression to transform
            -> Expression -- ^ transformed expression
addArgument func newArg expr = case expr of
    Assignment v e -> Assignment v (addArgument func newArg e)
    FunctionCall fname@(FuncName fname') e args b
        | fname' == func ->
            FunctionCall fname e (newArg:args) b
    _ -> expr

-- | Checks if a variable is used in any of the given expressions
--
-- See 'isVarUsed1'
isVarUsed :: Variable -> [(Int, Expression)] -> Bool
isVarUsed v = any (isVarUsed1 v . snd)


-- | Checks if a variable is used in a single 'Expression'
--
-- See 'isVarUsed'
isVarUsed1 :: Variable -> Expression -> Bool
isVarUsed1 v expr = evalCont $ callCC $ \exit -> do
                recursiveAnalyse (isVarUsed1' exit) expr
                return False
    where
        isVarUsed1' :: (Bool -> Cont Bool ()) -> Expression -> Cont Bool ()
        isVarUsed1' exit (Lookup _ v')
            | v == v' = exit True
        isVarUsed1' _ _ = return ()

{- If a variable is not used after a call to write(), we can destroy it.
    This is implemented by adding the argument __can_move=True to
    write() calls -}
writeToMove :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
writeToMove = return . writeToMove' []
writeToMove' _ [] = []
writeToMove' blocked ((lno,expr):rest) = (lno, addMove toRemove expr):writeToMove' blocked' rest
    where
        toRemove = filter (`notElem` blocked) unused
        unused = filter (not . flip isVarUsed rest) $ functionVars "write" expr
        blocked' = blockhere ++ blocked
        blockhere = case expr of
                      Assignment var (FunctionCall (FuncName fname) _ _ _)
                        | fname `elem` ["fastq", "paired", "samfile"] -> [var]
                      Assignment var (Lookup _ prev)
                        | prev `elem` blocked -> [var]
                      _ -> []
        addMove :: [Variable] -> Expression -> Expression
        addMove dead = pureRecursiveTransform addMove'
            where
                addMove' (FunctionCall f@(FuncName "write") e@(Lookup _ v) args b)
                    | v `elem` dead = FunctionCall f e ((Variable "__can_move", ConstBool True):args) b
                addMove' e = e

-- | Variables used in calling the function func
functionVars :: T.Text -- ^ function name
                -> Expression -- expression to analyse
                -> [Variable]
functionVars fname expr = execWriter (recursiveAnalyse fvars expr)
    where
        fvars :: Expression -> Writer [Variable] ()
        fvars (FunctionCall (FuncName fname') (Lookup _ v) _ _)
            | fname' == fname = tell [v]
        fvars _ = return ()

qcInPreprocess :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
qcInPreprocess [] = return []
qcInPreprocess ((lno,expr):rest) = case fastQVar expr of
        Nothing -> ((lno,expr):) <$> qcInPreprocess rest
        Just (fname, v) -> if not $ canQCPreprocessTransform v rest
                    then ((lno,expr):) <$> qcInPreprocess rest
                    else do
                        let expr' = addArgument fname (Variable "__perform_qc", ConstBool False) expr
                            rest' = rewritePreprocess v rest
                        outputListLno' TraceOutput ["Transformation for QC triggered for variable ", show v, " on line ", show lno, "."]
                        ((lno, expr'):) <$> qcInPreprocess rest'

rewritePreprocess _ [] = [] -- this should never happen
rewritePreprocess v ((lno,expr):rest) = case expr of
    Assignment t (FunctionCall f@(FuncName "preprocess") e@(Lookup _ v') args b)
        | v == v' ->
                let expr' = FunctionCall f e ((Variable "__input_qc", ConstBool True):args) b
                    in (lno,Assignment t expr'):rest
    _ -> (lno,expr):rewritePreprocess v rest

fastQVar :: Expression -> Maybe (T.Text, Variable)
fastQVar (Assignment v (FunctionCall (FuncName fname) _ _ _))
        | fname `elem` ["fastq", "paired", "load_mocat_sample"] = Just (fname, v)
fastQVar _ = Nothing

-- The rule is: we can perform the transform if the first usage of the Variable
-- 'v' is in a preproces call. Otherwise, it is not guaranteed to be safe
canQCPreprocessTransform :: Variable -> [(Int, Expression)] -> Bool
canQCPreprocessTransform _ [] = False
canQCPreprocessTransform v ((_,Assignment _ (FunctionCall (FuncName "preprocess") (Lookup _ v') _ _)):_)
    | v' == v = True
canQCPreprocessTransform v ((_, expr):rest)
    | isVarUsed1 v expr = False
    | otherwise = canQCPreprocessTransform v rest


-- | 'ifLenDiscardSpecial' special cases a common case inside preprocess
-- blocks, namely:
--
-- if len(read) < #:
--     discard
--
-- gets rewritten to
--
-- Optimized (LenThresholdDiscard read < #)
--
ifLenDiscardSpecial :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
ifLenDiscardSpecial = pureTransform $ \case
        (Condition (BinaryOp b (UnaryOp UOpLen (Lookup _ v)) (ConstInt thresh))
                                    (Sequence [Discard])
                                    (Sequence []))
            | b `elem` [BOpLT, BOpLTE, BOpGT, BOpGTE] -> Optimized (LenThresholdDiscard v b (fromInteger thresh))
        e -> e

substrimReassign :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
substrimReassign = pureTransform $ \case
        (Assignment v (FunctionCall (FuncName "substrim") (Lookup _ v') [(Variable "min_quality", ConstInt mq)] Nothing))
            | v == v' -> Optimized (SubstrimReassign v (fromInteger mq))
        e -> e


-- | 'addFileChecks' implements the following transformation
--
-- variable = <non constant expression>
--
-- <code>
--
-- write(input, ofile="output/"+variable+".sam")
--
-- into
--
-- variable = <non constant expression>
-- __check_ofile("output/"+variable+".sam")
--
-- <code>
--
-- write(input, ofile="output/"+variable+".sam")
addFileChecks :: [(Int,Expression)] -> NGLessIO [(Int, Expression)]
                    -- this is easier to do on the reversed script
addFileChecks sc = reverse <$> (checkIFiles (reverse sc) >>= checkOFiles)
    where
        -- This could be combined into a single pass
        -- For script preprocessing, we generally disregard performance, however
        checkIFiles = addFileChecks' "__check_ifile" ArgCheckFileReadable
        checkOFiles = addFileChecks' "__check_ofile" ArgCheckFileWritable

addFileChecks' :: T.Text -> ArgCheck -> [(Int,Expression)] -> NGLessIO [(Int, Expression)]
addFileChecks' _ _ [] = return []
addFileChecks' checkFname tag ((lno,e):rest) = do
        mods <- loadedModules
        vars <- runNGLess $ execWriterT (recursiveAnalyse (getFileExpressions mods) e)
        rest' <- addFileChecks' checkFname tag (addCheck vars (maybeAddChecks vars rest))
        return ((lno,e):rest')

     where
        addCheck [(_, oexpr)] = ((lno, checkFileExpression oexpr):)
        addCheck _ = id

        maybeAddChecks :: [(Variable,Expression)] -> [(Int, Expression)] -> [(Int, Expression)]
        maybeAddChecks _ [] = []
        maybeAddChecks vars@[(v,complete)] ((lno',e'):rest') = case e' of
            Assignment v' _
                | v' == v -> ((lno', checkFileExpression complete):(lno', e'):rest')
            _ -> (lno',e') : maybeAddChecks vars rest'
        maybeAddChecks _ rest' = rest'

        checkFileExpression complete = FunctionCall
                            (FuncName checkFname)
                            complete
                            [(Variable "original_lno", ConstInt (toInteger lno))]
                            Nothing

        -- returns the variables used and expressions that depend on them
        getFileExpressions :: [Module] -> Expression -> (WriterT [(Variable,Expression)] NGLess) ()
        getFileExpressions mods (FunctionCall f expr args _) = case findFunction mods f of
            Just finfo -> do
                when (tag `elem` funcArgChecks finfo) $
                    extractExpressions (Just expr)
                forM_ (funcKwArgs finfo) $ \ainfo ->
                    when (tag `elem` argChecks ainfo) $
                        extractExpressions (lookup (Variable $ argName ainfo) args)
            Nothing -> throwShouldNotOccur ("Transform.getFileExpressions: Unknown function: " ++ show f ++ ". This should have been caught before")
        getFileExpressions _ _ = return ()

        extractExpressions :: (MonadWriter [(Variable, Expression)] m) =>  Maybe Expression -> m ()
        extractExpressions (Just ofile) = case ofile of
            BinaryOp _ re le -> case uniq (validVariables re ++ validVariables le) of
                [v] -> tell [(v, ofile)]
                _ -> return ()
            Lookup _ v -> tell [(v, ofile)]
            _ -> return ()
        extractExpressions Nothing = return ()

        validVariables (Lookup _ v) = [v]
        validVariables (BinaryOp _ re le) = validVariables re ++ validVariables le
        validVariables (ConstStr _) = []
        validVariables _ = [Variable "this", Variable "wont", Variable "work"] -- this causes the caller to bailout

-- | 'addIndexChecks' implements the following transformation
--
-- array = <non constant expression>
--
-- <code>
--
--   array[ix]
--
-- into
--
-- array = <non constant expression>
-- __check_index_access(array, index1=ix,...)
--
-- <code>
--
-- write(input, ofile="output/"+variable+".sam")
addIndexChecks :: [(Int,Expression)] -> NGLessIO [(Int, Expression)]
addIndexChecks sc = reverse <$> addIndexChecks' (reverse sc)
addIndexChecks' :: [(Int,Expression)] -> NGLessIO [(Int, Expression)]
addIndexChecks' [] = return []
addIndexChecks' ((lno,e):rest) = do
        vars <- runNGLess . execWriterT . flip recursiveAnalyse e $ \case
            (IndexExpression (Lookup _ v) (IndexOne ix1@ConstInt{})) -> tell [(v, ix1)]
            _ -> return ()
        rest' <- addIndexChecks' (maybeAddChecks vars rest)
        return ((lno,e):rest')

     where
        -- The similarity of this code and the code for addFileChecks hints at
        -- a possible merging with a good abstraction
        maybeAddChecks :: [(Variable,Expression)] -> [(Int, Expression)] -> [(Int, Expression)]
        maybeAddChecks [(v,ix)] [] = [(0, indexCheckExpr v ix)]
        maybeAddChecks vars@[(v,ix)] ((lno',e'):rest') = case e' of
            Assignment v' _
                | v' == v -> ((lno', indexCheckExpr v ix):(lno', e'):rest')
            _ -> (lno',e') : maybeAddChecks vars rest'
        maybeAddChecks _ rest' = rest'

        indexCheckExpr arr ix1 = FunctionCall
                            (FuncName "__check_index_access")
                            (Lookup Nothing arr)
                            [(Variable "original_lno", ConstInt (toInteger lno))
                            ,(Variable "index1", ix1)]
                            Nothing


-- | Implements addition of temp$nn variables to simplify expressions
--
-- This allows the rest of the code to be simpler
addTemporaries = addTemporaries' 0
    where
        addTemporaries' :: Int -> [(Int,Expression)] -> NGLessIO [(Int,Expression)]
        addTemporaries' _ [] = return []
        addTemporaries' next ((lno,e):rest) = do
                mods <- loadedModules
                let (next', es) = addTemporaries1 mods next e
                rest' <- addTemporaries' next' rest
                let lno_e' = (lno,) <$> es
                return $ lno_e' ++ rest'

        {- This is incomplete:
         - The transformation should also process the expressions inside blocks
         -}
        addTemporaries1 :: [Module] -> Int -> Expression -> (Int, [Expression])
        addTemporaries1 _ next e@(FunctionCall _ _ _ (Just _)) = (next, [e])
        addTemporaries1 _ next e@(Assignment _ (FunctionCall _ _ _ (Just _))) = (next, [e])
        addTemporaries1 mods next expr = let (e', next', pre) = runRWS (recursiveTransform functionCallTemp expr) () next in
                                                    (next', combineExpr pre e')
            where
                isAssignTo v (Assignment v' _) = v == v'
                isAssignTo _ _ = False

                findDrop :: [a] -> (a -> Bool) -> Maybe ([a], a)
                findDrop [] _ = Nothing
                findDrop (x:xs) f
                    | f x = Just (xs, x)
                    | otherwise = first (x:) <$> findDrop xs f

                combineExpr :: [Expression] -> Expression -> [Expression]
                combineExpr pre (Lookup _ v) = case findDrop pre (isAssignTo v) of
                    Just (pre', Assignment _ e') -> combineExpr pre' e'
                    _ -> error "This is impossible"
                combineExpr pre (Assignment v' (Lookup _ vt@(Variable t)))
                    | T.isPrefixOf "temp$" t = case findDrop pre (isAssignTo vt) of
                        Just (pre', Assignment _ e) -> pre' ++ [Assignment v' e]
                        _ -> error "Impossible [combineExpr2]"
                combineExpr pre e' = pre ++ [e']

                functionCallTemp :: Expression -> RWS () [Expression] Int Expression
                functionCallTemp e@(FunctionCall f _ _ _) = do
                    n <- get
                    let v = Variable (T.pack $ "temp$"++show n)
                    put (n + 1)
                    tell [Assignment v e]
                    let t = funcRetType <$> findFunction mods f
                    return (Lookup t v)
                functionCallTemp e = return e

{-| Calculation of hashes for output method calls
 so that the hash depends only on the relevant (influencing the result) part of
 the script.

 Hashes for variables are stored in a map (as a state).  For each expression
 (top to bottom) first the block variables are added to the map (if present),
 then hashes are calculated and applied (in lookups) recursively.
 Each output call receives new variable __hash storing the hash of its own nput
 expression (with hashes already applied inside).
-}

addOutputHash :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
addOutputHash expr_lst = do
        nv <- ngleVersion <$> nglEnvironment
        modules <- loadedModules
        let modInfos = map modInfo modules
            state0 = M.insert (Variable "ARGV") (T.pack "ARGV") M.empty
            versionString = show nv ++ show (sortOn modName modInfos)
        return $! evalState (mapM (secondM $ addOutputHash' versionString) expr_lst) state0
    where
        addOutputHash' :: String -> Expression -> State (M.Map Variable T.Text) Expression
        addOutputHash' versionString expr = flip recursiveTransform expr $ \e -> case e of
                        Assignment v val -> do
                            h <- hashOf val
                            modify (M.insert v h)
                            return e
                        FunctionCall f@(FuncName fname) oarg kwargs block
                            | fname `elem` ["collect", "write"] -> do
                                h <- hashOf oarg
                                return (FunctionCall f oarg ((Variable "__hash", ConstStr h):kwargs) block)
                        _ -> return e
            where
                injectBlockVars :: Maybe Block -> M.Map Variable T.Text -> M.Map Variable T.Text
                injectBlockVars Nothing m = m
                injectBlockVars (Just (Block vars _)) m = foldl' injectBlockVars' m vars
                    where
                        injectBlockVars' hm v@(Variable n) = M.insert v n hm
                hashOf :: Expression -> State (M.Map Variable T.Text) T.Text
                hashOf e@(FunctionCall _ _ _ block) = withState (injectBlockVars block) $ hashOf' e
                hashOf e  = hashOf' e

                hashOf' ex = do
                    expr' <- flip recursiveTransform ex $ \case
                        Lookup t v@(Variable n) -> do
                            h <- fromMaybe n <$> gets (M.lookup v)
                            return $! Lookup t (Variable h)
                        e -> return e
                    return . T.pack . MD5.md5s . MD5.Str . (versionString ++) . show $ expr'

-- In ngless 0.0, preprocess() would change its arguments, so that
--
--  preprocess(input) ...
--
-- was equivalent to
--
-- input = preprocess(input) ...
reassignPreprocess :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
reassignPreprocess sc = do
    v <- ngleVersion <$> nglEnvironment
    return $! case v of
        NGLVersion 0 0 -> map (second reassignPreprocess') sc
        _ -> sc
reassignPreprocess' :: Expression -> Expression
reassignPreprocess' e@(FunctionCall (FuncName "preprocess") (Lookup _ v) _ _) = Assignment v e
reassignPreprocess' e = e
