{- Copyright 2016 NGLess Authors
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

import Language
import Modules
import Output
import NGLess
import BuiltinFunctions


{- Before interpretation, scripts are transformed to allow for several
    optimizations.

    This is implemented by adding hidden arguments to functions and by
    replacing expressions by Optimized instances.
-}

transform :: [Module] -> Script -> NGLessIO Script
transform mods sc = Script (nglHeader sc) <$> applyM transforms (nglBody sc)
    where
        applyM [] e = return e
        applyM (t:ts) e = t e >>= applyM ts
        transforms = preTransforms ++ modTransforms ++ builtinTransforms
        modTransforms = map modTransform mods
        preTransforms =
                [ addTemporaries
                ]
        builtinTransforms =
                [ writeToMove
                , qcInPreprocess
                , ifLenDiscardSpecial
                , substrimReassign
                , addOFileChecks
                ]

pureRecursiveTransform :: (Expression -> Expression) -> Expression -> Expression
pureRecursiveTransform f e = runIdentity (recursiveTransform (return . f) e)

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

isVarUsed :: Variable -> [(Int, Expression)] -> Bool
isVarUsed v = any (isVarUsed1 v . snd)

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
    FunctionCall f@(FuncName "preprocess") e@(Lookup _ v') args b
        | v == v' ->
                let expr' = FunctionCall f e ((Variable "__input_qc", ConstBool True):args) b
                    in (lno,expr'):rest
    _ -> (lno,expr):rewritePreprocess v rest

fastQVar :: Expression -> Maybe (T.Text, Variable)
fastQVar (Assignment v (FunctionCall (FuncName fname) _ _ _))
        | fname `elem` ["fastq", "paired"] = Just (fname, v)
fastQVar _ = Nothing

-- The rule is: we can perform the transform if the first usage of the Variable
-- 'v' is in a preproces call. Otherwise, it is not guaranteed to be safe
canQCPreprocessTransform :: Variable -> [(Int, Expression)] -> Bool
canQCPreprocessTransform _ [] = False
canQCPreprocessTransform v ((_,FunctionCall (FuncName "preprocess") (Lookup _ v') _ _):_)
    | v' == v = True
canQCPreprocessTransform v ((_, expr):rest)
    | isVarUsed1 v expr = False
    | otherwise = canQCPreprocessTransform v rest


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


-- | This makes the following transformation
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
addOFileChecks :: [(Int,Expression)] -> NGLessIO [(Int, Expression)]
addOFileChecks sc = reverse <$> addOFileChecks' (reverse sc) -- this is easier to do on the reversed script
addOFileChecks' :: [(Int,Expression)] -> NGLessIO [(Int, Expression)]
addOFileChecks' [] = return []
addOFileChecks' ((lno,e):rest) = do
        mods <- loadedModules
        vars <- runNGLess $ execWriterT (recursiveAnalyse (getOFileExpressions mods) e)
        rest' <- addOFileChecks' (maybeAddChecks vars rest)
        return ((lno,e):rest')

     where
        maybeAddChecks :: [(Variable,Expression)] -> [(Int, Expression)] -> [(Int, Expression)]
        maybeAddChecks _ [] = []
        maybeAddChecks vars@[(v,complete)] ((lno',e'):rest') = case e' of
            Assignment v' _
                | v' == v -> ((lno', checkExpression complete):(lno', e'):rest')
            _ -> (lno',e') : maybeAddChecks vars rest'
        maybeAddChecks _ rest' = rest'

        checkExpression complete = FunctionCall
                            (FuncName "__check_ofile")
                            complete
                            [(Variable "original_lno", ConstInt (toInteger lno))]
                            Nothing

        -- returns the variables used and expressions that depend on them
        getOFileExpressions :: [Module] -> Expression -> (WriterT [(Variable,Expression)] NGLess) ()
        getOFileExpressions mods (FunctionCall f expr args _) = case findFunction mods f of
            Just finfo -> do
                when (ArgCheckFileWritable `elem` funcArgChecks finfo) $
                    extractExpressions (Just expr)
                forM_ (funcKwArgs finfo) $ \ainfo ->
                    when (ArgCheckFileWritable `elem` argChecks ainfo) $
                        extractExpressions (lookup (Variable $ argName ainfo) args)
            Nothing -> throwShouldNotOccur ("Transform.getOFileExpressions: Unknown function: " ++ show f ++ ". This should have been caught before")
        getOFileExpressions _ _ = return ()

        extractExpressions :: (MonadWriter [(Variable, Expression)] m) =>  Maybe Expression -> m ()
        extractExpressions (Just ofile) = case ofile of
            BinaryOp _ re le -> case validVariables re ++ validVariables le of
                [v] -> tell [(v, ofile)]
                _ -> return ()
            _ -> return ()
        extractExpressions Nothing = return ()

        validVariables (Lookup _ v) = [v]
        validVariables (BinaryOp _ re le) = validVariables re ++ validVariables le
        validVariables (ConstStr _) = []
        validVariables _ = [Variable "this", Variable "wont", Variable "work"] -- this causes the caller to bailout

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
