{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpret
    ( interpret
    , _evalIndex
    , _evalUnary
    , _evalBinary
    ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit as C
import           Data.Conduit (($=), (=$=), (=$), ($$+), ($$+-))
import           Data.Conduit.Async ((=$=&), ($$&))
import qualified Data.Vector as V
import           Control.DeepSeq (NFData(..))

import Data.IORef
import System.IO
import System.Directory
import Data.List (find)
import GHC.Conc                 (getNumCapabilities)

import Substrim
import Language
import FileManagement
import Output
import Modules
import NGLess
import Data.Sam
import Data.FastQ

import Interpretation.Map
import Interpretation.Count
import Interpretation.FastQ
import Interpretation.Write
import Interpretation.Select
import Interpretation.Unique
import Utils.Samtools
import Utils.Utils
import Utils.Conduit


{- Interpretation is done inside 3 and a half monads
 -  1. InterpretationEnvIO
 -      This is the NGLessIO monad with a variable environment on top
 -  2. InterpretationEnv
 -      This is the read-write variable environment
 -  3. InterpretationROEnv
 -      This is a read-only variable environment.
 -  3½. Either NGError
 -      Pure functions are in the 'Either NGError' monad
 - 
 - Monad (1) is a superset of (2) which is a superset of (3). runInEnv and
 - friends switch between the monads.
 -
 - For blocks, we have a special system where block-variables are read-write,
 - but others are read-only.
 -
 - Functions inside the interpret monads are named interpret*, helper
 - non-monadic functions which perform computations are named eval*.
 -
 -}


data NGLInterpretEnv = NGLInterpretEnv
    { ieModules :: [Module]
    , ieVariableEnv :: Map.Map T.Text NGLessObject
    }

type InterpretationEnvT m =
                (StateT NGLInterpretEnv m)
-- Monad 1: IO + read-write environment
type InterpretationEnvIO = InterpretationEnvT NGLessIO
-- Monad 2: read-write environment
type InterpretationEnv = InterpretationEnvT (ExceptT NGError Identity)
-- Monad 3: read-only environment
type InterpretationROEnv = ExceptT NGError (Reader NGLInterpretEnv)

{- The result of a block is a status indicating why the block finished
 - and the value of all block variables.
 -}
data BlockStatus = BlockOk | BlockDiscarded | BlockContinued
    deriving (Eq,Show)

data BlockResult = BlockResult
                { blockStatus :: {-# UNPACK #-} !BlockStatus
                , blockValues :: [(T.Text, NGLessObject)]
                } deriving (Eq,Show)

autoComprehendNB f (NGOList es) args = NGOList <$> sequence [f e args | e <- es]
autoComprehendNB f e args = f e args

-- Set line number
setlno :: Int -> InterpretationEnvIO ()
setlno !n = liftIO $ setOutputLno (Just n)

lookupVariable :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupVariable !k = (liftM2 (<|>))
    (lookupConstant k)
    (Map.lookup k . ieVariableEnv <$> ask)

lookupConstant :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupConstant !k = do
    constants <- concat . map modConstants . ieModules <$> ask
    case filter ((==k) . fst) constants of
        [] -> return Nothing
        [(_,v)] -> return (Just v)
        _ -> throwShouldNotOccur ("Multiple hits found for constant " ++ T.unpack k)



setVariableValue :: T.Text -> NGLessObject -> InterpretationEnvIO ()
setVariableValue !k !v = modify $ \(NGLInterpretEnv mods e) -> (NGLInterpretEnv mods (Map.insert k v e))

getName (FuncName f) = f

findFunction :: FuncName -> InterpretationEnvIO (NGLessObject -> KwArgsValues -> NGLessIO NGLessObject)
findFunction fname = do
        mods <- gets ieModules
        case filter hasF mods of
            [m] -> do
                let Just func = find ((== fname) . funcName) (modFunctions m)
                    wrap = if funcAllowsAutoComprehension func
                                then autoComprehendNB
                                else id
                return $ wrap $ (runFunction m) (getName fname)
            [] -> throwShouldNotOccur . T.unpack $ T.concat ["Function '", getName fname, "' not found (not builtin and not in any loaded module)"]
            ms -> throwShouldNotOccur . T.unpack $ T.concat (["Function '", T.pack $ show fname, "' found in multiple modules! ("] ++ [T.concat [modname, ":"] | modname <- modName . modInfo <$> ms])
    where
        hasF m = (fname `elem` (funcName `fmap` modFunctions m))

runInterpretationRO :: NGLInterpretEnv -> InterpretationROEnv a -> Either NGError a
runInterpretationRO env act = runReader (runExceptT act) env

runInEnv :: InterpretationEnv a -> InterpretationEnvIO a
runInEnv act = do
    env <- gets id
    let Identity r = runExceptT (runStateT act env)
    case r of
        Left e -> throwError e
        Right (v, env') -> do
            put env'
            return v

runInROEnv :: InterpretationROEnv a -> InterpretationEnv a
runInROEnv action = do
    env <- gets id
    case runInterpretationRO env action of
        Left e -> throwError e
        Right v -> return v

runInROEnvIO :: InterpretationROEnv a -> InterpretationEnvIO a
runInROEnvIO = runInEnv . runInROEnv

runEitherInROEnv :: Either NGError a -> InterpretationROEnv a
runEitherInROEnv (Right v) = return v
runEitherInROEnv (Left err) = throwError err

runNGLessIO :: NGLessIO a -> InterpretationEnvIO a
runNGLessIO act = do
    r <- liftResourceT (runExceptT act)
    case r of
        Right val -> return val
        Left err -> throwError err

-- | By necessity, this code has several unreachable corners

unreachable :: ( MonadError NGError m) => String -> m a
unreachable err = throwShouldNotOccur ("Reached code that was thought to be unreachable!\n"++err)
nglTypeError err = throwShouldNotOccur ("Unexpected type error! This should have been caught by validation!\n"++err)

traceExpr m e =
    runNGLessIO $ outputListLno' TraceOutput ["Interpreting [", m , "]: ", show e]

interpret :: [Module] -> [(Int,Expression)] -> NGLessIO ()
interpret modules es = do
    evalStateT (interpretIO es) (NGLInterpretEnv modules Map.empty)
    outputListLno InfoOutput Nothing ["Interpretation finished."]

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO es = forM_ es $ \(ln,e) -> do
    setlno ln
    traceExpr "interpretIO" e
    interpretTop e

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = traceExpr "assignment" val >> interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ interpretFunction f e args b
interpretTop (Condition c ifTrue ifFalse) = do
    c' <- runInROEnvIO (interpretExpr c >>= boolOrTypeError "interpreting if condition")
    interpretTop (if c'
        then ifTrue
        else ifFalse)
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop _ = throwShouldNotOccur "Top level statement is NOP"

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = interpretFunction f e args b
interpretTopValue (ListExpression es) = NGOList <$> mapM interpretTopValue es
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup (Variable v)) = lookupVariable v >>= \case
        Nothing -> throwScriptError "Variable lookup error"
        Just r' -> return r'
interpretExpr (BuiltinConstant (Variable "STDIN")) = return (NGOString "/dev/stdin")
interpretExpr (BuiltinConstant (Variable "STDOUT")) = return (NGOString "/dev/stdout")
interpretExpr (BuiltinConstant (Variable v)) = throwShouldNotOccur ("Unknown builtin constant '" ++ show v ++ "': it should not have been accepted.")
interpretExpr (ConstStr t) = return (NGOString t)
interpretExpr (ConstBool b) = return (NGOBool b)
interpretExpr (ConstSymbol s) = return (NGOSymbol s)
interpretExpr (ConstInt n) = return (NGOInteger n)
interpretExpr (ConstDouble n) = return (NGODouble n)
interpretExpr (UnaryOp op v) = do
    v' <- interpretExpr v
    runEitherInROEnv (_evalUnary op v')
interpretExpr (BinaryOp bop v1 v2) = do
    v1' <- interpretExpr v1
    v2' <- interpretExpr v2
    runEitherInROEnv (_evalBinary bop v1' v2')
interpretExpr (IndexExpression expr ie) = do
    expr' <- interpretExpr expr
    ie' <- interpretIndex ie
    runEitherInROEnv (_evalIndex expr' ie')
interpretExpr (ListExpression e) = NGOList <$> mapM interpretExpr e
interpretExpr (MethodCall met self arg args) = do
    self' <- interpretExpr self
    arg' <- fmapMaybeM interpretExpr arg
    args' <- forM args $ \(Variable v, e) -> do
        e' <- interpretExpr e
        return (v, e')
    executeMethod met self' arg' args'
interpretExpr not_expr = throwShouldNotOccur ("Expected an expression, received " ++ show not_expr ++ " (in interpretExpr)")

interpretIndex :: Index -> InterpretationROEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: Maybe Expression -> InterpretationROEnv (Maybe NGLessObject)
maybeInterpretExpr = fmapMaybeM interpretExpr

interpretFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
interpretFunction (FuncName "preprocess") expr@(Lookup (Variable varName)) args (Just block) = do
    expr' <- runInROEnvIO $ interpretExpr expr
    args' <- interpretArguments args
    res' <- executePreprocess expr' args' block
    setVariableValue varName res'
    return res'
interpretFunction (FuncName "preprocess") expr _ _ = throwShouldNotOccur ("preprocess expected a variable holding a NGOReadSet, but received: " ++ show expr)
interpretFunction f expr args block = do
    expr' <- interpretTopValue expr
    args' <- interpretArguments args
    interpretFunction' f expr' args' block

interpretFunction' :: FuncName -> NGLessObject -> KwArgsValues -> Maybe Block -> InterpretationEnvIO NGLessObject
interpretFunction' (FuncName "fastq")     expr args Nothing = runNGLessIO (executeFastq expr args)
interpretFunction' (FuncName "group")     expr args Nothing = runNGLessIO (executeGroup expr args)
interpretFunction' (FuncName "samfile")   expr args Nothing = autoComprehendNB executeSamfile expr args
interpretFunction' (FuncName "unique")    expr args Nothing = runNGLessIO (executeUnique expr args)
interpretFunction' (FuncName "write")     expr args Nothing = traceExpr "write" expr >> runNGLessIO (executeWrite expr args)
interpretFunction' (FuncName "map")       expr args Nothing = runNGLessIO (executeMap expr args)
interpretFunction' (FuncName "mapstats")  expr args Nothing = runNGLessIO (executeMapStats expr args)
interpretFunction' (FuncName "select")    expr args Nothing = runNGLessIO (executeSelect expr args)
interpretFunction' (FuncName "count")     expr args Nothing = runNGLessIO (executeCount expr args)
interpretFunction' (FuncName "print")     expr args Nothing = executePrint expr args
interpretFunction' (FuncName "paired")   mate1 args Nothing = runNGLessIO (executePaired mate1 args)
interpretFunction' (FuncName "select")    expr args (Just b) = executeSelectWBlock expr args b
interpretFunction' fname@(FuncName fname') expr args Nothing = do
    traceExpr ("executing module function: '"++T.unpack fname'++"'") expr
    execF <- findFunction fname
    runNGLessIO (execF expr args) >>= \case
        NGOExpression expr' -> interpretTopValue expr'
        val -> return val
interpretFunction' f _ _ _ = throwShouldNotOccur . concat $ ["Interpretation of ", show f, " is not implemented"]

executeSamfile expr@(NGOString fname) args = do
    traceExpr "samfile" expr
    gname <- lookupStringOrScriptErrorDef (return fname) "samfile group name" "name" args
    return $ NGOMappedReadSet gname (T.unpack fname) Nothing
executeSamfile e args = unreachable ("executeSamfile " ++ show e ++ " " ++ show args)

data PreprocessPairOutput = Pair !ShortRead !ShortRead | Single !ShortRead
instance NFData PreprocessPairOutput where
    rnf (Pair _ _) = ()
    rnf (Single _) = ()

splitPreprocessPair :: V.Vector PreprocessPairOutput -> (V.Vector ShortRead, V.Vector ShortRead, V.Vector ShortRead)
splitPreprocessPair input = (vMapMaybe extract1 input, vMapMaybe extract2 input, vMapMaybe extractS input)
    where
        extract1 = \case
            Pair sr _ -> Just sr
            _ -> Nothing
        extract2 = \case
            Pair _ sr -> Just sr
            _ -> Nothing
        extractS = \case
            Single sr -> Just sr
            _ -> Nothing

vMapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
vMapMaybe f v = V.unfoldr loop 0
    where
        loop i
            | i < V.length v = case f (v V.! i) of
                                    Just val -> Just (val, i + 1)
                                    Nothing -> loop (i + 1)
            | otherwise = Nothing

vMapMaybeLifted :: (a -> Either e (Maybe b)) -> V.Vector a -> Either e (V.Vector b)
vMapMaybeLifted f v = sequence $ V.unfoldr loop 0
    where
        loop i
            | i < V.length v = let !n = i+1 in
                            case f (v V.! i) of
                                Right Nothing -> loop n
                                Right (Just val) -> Just (Right val, n)
                                Left err -> Just (Left err, V.length v) -- early exit: we are done
            | otherwise = Nothing

inlineQCIf False _ = C.awaitForever C.yield -- identity conduit
inlineQCIf True resVar = C.passthroughSink (C.transPipe runNGLessIO (getPairedLines $= fqStatsC)) (liftIO . writeIORef resVar . Just)

executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block = NGOList <$> mapM (\x -> executePreprocess x args _block) e
executePreprocess (NGOReadSet name (ReadSet1 enc file)) args (Block [Variable var] block) = do
        runNGLessIO $ outputListLno' DebugOutput ["Preprocess on ", file]
        (newfp, h) <- runNGLessIO $ openNGLTempFile file "preprocessed_" "fq.gz"
        qcInput <- lookupBoolOrScriptErrorDef (return False) "preprocess" "__qc_input" args
        qcPre <- liftIO $ newIORef (Nothing :: Maybe FQStatistics)
        qcPost <- liftIO $ newIORef (Nothing :: Maybe FQStatistics)

        env <-gets id
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        (conduitPossiblyCompressedFile file
            =$= linesC
            =$= inlineQCIf qcInput qcPre
            =$= fqConduitR enc
            =$= C.conduitVector 8192)
            =$=& (asyncMapEitherC mapthreads (vMapMaybeLifted ((liftM (liftM $ fqEncode enc)) . runInterpretationRO env . interpretPBlock1 block var))
                    =$= (C.concat :: C.Conduit (V.Vector B.ByteString) InterpretationEnvIO B.ByteString)
                    =$= C.passthroughSink (C.transPipe runNGLessIO (linesC =$= getPairedLines =$= fqStatsC)) (liftIO . writeIORef qcPost . Just)
                    =$= C.conduitVector 4000
                    )
                $$& (
                    (C.concat :: C.Conduit (V.Vector B.ByteString) InterpretationEnvIO B.ByteString)
                    =$= C.gzip =$ C.sinkHandle h)
        liftIO (hClose h)
        when qcInput $ do
            Just qcPreStats <- liftIO $ readIORef qcPre
            runNGLessIO $ outputFQStatistics (T.unpack name) qcPreStats enc
        Just qcPostStats <- liftIO $ readIORef qcPost
        runNGLessIO $ outputFQStatistics (T.unpack name ++ "-preprocessed") qcPostStats enc
        return (NGOReadSet name $ ReadSet1 enc newfp)

executePreprocess (NGOReadSet name (ReadSet2 enc fp1 fp2)) _args block = executePreprocess (NGOReadSet name (ReadSet3 enc fp1 fp2 "")) _args block
executePreprocess (NGOReadSet name (ReadSet3 enc fp1 fp2 fp3)) args (Block [Variable var] block) = do
        keepSingles <- runNGLessIO $ lookupBoolOrScriptErrorDef (return True) "preprocess argument" "keep_singles" args
        qcInput <- lookupBoolOrScriptErrorDef (return False) "preprocess" "__qc_input" args

        runNGLessIO $ outputListLno' DebugOutput (["Preprocess on paired end ",
                                                fp1, " + ", fp2] ++ (if fp3 /= ""
                                                                    then [" with singles ", fp3]
                                                                    else []))
        (fp1', out1) <- runNGLessIO $ openNGLTempFile fp1 "preprocessed.1." ".fq.gz"
        (fp2', out2) <- runNGLessIO $ openNGLTempFile fp2 "preprocessed.2." ".fq.gz"
        (fps, out3) <- runNGLessIO $ openNGLTempFile fp1 "preprocessed.singles." ".fq.gz"

        qcPre1 <- liftIO $ newIORef (Nothing :: Maybe FQStatistics)
        qcPre2 <- liftIO $ newIORef (Nothing :: Maybe FQStatistics)
        qcPre3 <- liftIO $ newIORef (Nothing :: Maybe FQStatistics)

        let rs1 :: C.Source InterpretationEnvIO ShortRead
            rs1 = conduitPossiblyCompressedFile fp1 =$= linesC =$= inlineQCIf qcInput qcPre1 =$= fqConduitR enc

            rs2 :: C.Source InterpretationEnvIO ShortRead
            rs2 = conduitPossiblyCompressedFile fp2 =$= linesC =$= inlineQCIf qcInput qcPre2 =$= fqConduitR enc

            rs3 :: C.Source InterpretationEnvIO ShortRead
            rs3 = if fp3 /= ""
                    then conduitPossiblyCompressedFile fp3 =$= linesC =$= inlineQCIf qcInput qcPre3 =$= fqConduitR enc
                    else C.yieldMany []

            write :: Handle -> C.Sink (V.Vector ShortRead) InterpretationEnvIO FQStatistics
            write h =
                    (C.concat :: C.Conduit (V.Vector ShortRead) InterpretationEnvIO ShortRead)
                    =$= snd <$> (zipSink2
                        (fqEncodeC enc =$= asyncGzipTo h)
                        (CL.map (\(ShortRead _ bps qs) -> (ByteLine bps, ByteLine qs)) =$= C.transPipe runNGLessIO fqStatsC))

        env <- gets id
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 2)
        [_, _, s3] <-
            (zipSource2 rs1 rs2
                =$= C.conduitVector 8192
                =$= asyncMapEitherC mapthreads (liftM splitPreprocessPair . vMapMaybeLifted (runInterpretationRO env . intercalate keepSingles))
                $$& C.sequenceSinks
                        [CL.map (\(a,_,_) -> a) =$= write out1
                        ,CL.map (\(_,a,_) -> a) =$= write out2
                        ,CL.map (\(_,_,a) -> a) =$= write out3
                        ])
        s3' <- rs3
                =$= preprocBlock
                =$= CL.mapMaybe (\case
                                Single r -> Just r
                                _ -> Nothing)
                =$= C.conduitVector 4096
                $$& write out3
        let n3 = nSeq s3
            n3' = nSeq s3'
            anySingle = n3 > 0 || n3' > 0
        liftIO $ forM_ [out1, out2, out3] hClose
        unless anySingle
            (liftIO $ removeFile fps)
        forM_ (zip [qcPre1, qcPre2, qcPre3] [fp1, fp2, fp3]) $ \(q,fp) ->
            liftIO (readIORef q) >>= \case
                Nothing -> return ()
                Just qcPreStats -> runNGLessIO $ outputFQStatistics fp qcPreStats enc

        runNGLessIO $ outputLno' DebugOutput "Preprocess finished"
        return . NGOReadSet name $ if anySingle
                    then ReadSet3 enc fp1' fp2' fps
                    else ReadSet2 enc fp1' fp2'
    where
        preprocBlock :: C.Conduit ShortRead InterpretationEnvIO PreprocessPairOutput
        preprocBlock = CL.mapMaybeM $ \r -> do
            r' <- runInROEnvIO $ interpretPBlock1 block var r
            return (Single <$> r')


        intercalate :: Bool -> (ShortRead, ShortRead) -> InterpretationROEnv (Maybe PreprocessPairOutput)
        intercalate keepSingles (r1, r2) = do
                r1' <- interpretPBlock1 block var r1
                r2' <- interpretPBlock1 block var r2
                return $ case (r1',r2') of
                    (Just r1'', Just r2'') -> Just (Pair r1'' r2'')
                    (Just r, Nothing) -> if keepSingles
                                                then Just (Single r)
                                                else Nothing
                    (Nothing, Just r) -> if keepSingles
                                                then Just (Single r)
                                                else Nothing
                    (Nothing, Nothing) -> Nothing
executePreprocess v _ _ = unreachable ("executePreprocess: Cannot handle this input: " ++ show v)

executeMethod :: MethodName -> NGLessObject -> Maybe NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationROEnv NGLessObject
executeMethod method (NGOMappedRead samline) arg kwargs = runNGLess (executeMappedReadMethod method samline arg kwargs)
executeMethod m self arg kwargs = throwShouldNotOccur ("Method " ++ show m ++ " with self="++show self ++ " arg="++ show arg ++ " kwargs="++show kwargs ++ " is not implemented")


interpretPBlock1 :: Expression -> T.Text -> ShortRead -> InterpretationROEnv (Maybe ShortRead)
interpretPBlock1 block var r = do
    r' <- interpretBlock1 [(var, NGOShortRead r)] block
    case blockStatus r' of
        BlockDiscarded -> return Nothing -- Discard Read.
        _ -> case lookup var (blockValues r') of
                Just (NGOShortRead rr) -> case srLength rr of
                    0 -> return Nothing
                    _ -> return (Just rr)
                _ -> nglTypeError ("Expected variable "++show var++" to contain a short read.")

executePrint :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executePrint (NGOString s) [] = liftIO (T.putStr s) >> return NGOVoid
executePrint err  _ = throwScriptError ("Cannot print " ++ show err)

executeSelectWBlock :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executeSelectWBlock input@NGOMappedReadSet{ nglSamFile = samfp} [] (Block [Variable var] body) = do
        runNGLessIO $ outputListLno' TraceOutput ["Executing blocked select on file ", samfp]
        (oname, ohandle) <- runNGLessIO $ openNGLTempFile samfp "block_selected_" "sam"
        env <- gets id
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 2)
        (samcontent, ()) <-
            C.transPipe runNGLessIO (samBamConduit samfp)
                =$= linesC
                $$+ C.takeWhile ((=='@') . B8.head . unwrapByteLine)
                =$= CL.map unwrapByteLine
                =$= C.unlinesAscii
                =$= CB.sinkHandle ohandle
        samcontent
            $$+- C.transPipe runNGLessIO (readSamGroupsC' mapthreads)
            =$= asyncMapEitherC mapthreads (liftM concatLines . V.mapM (runInterpretationRO env . filterGroup))
            =$= CB.sinkHandle ohandle
        liftIO $ hClose ohandle
        return input { nglSamFile = oname }
    where
        concatLines :: V.Vector [B.ByteString] -> B.ByteString
        concatLines = B8.unlines . concat . V.toList

        filterGroup :: [SamLine] -> InterpretationROEnv [B.ByteString]
        filterGroup [] = return []
        filterGroup [SamHeader line] = return [line]
        filterGroup mappedreads  = do
                    mrs' <- interpretBlock1 [(var, NGOMappedRead mappedreads)] body
                    if blockStatus mrs' `elem` [BlockContinued, BlockOk]
                        then case lookup var (blockValues mrs') of
                            Just (NGOMappedRead []) -> return []
                            Just (NGOMappedRead rs) -> return (encodeSamLine <$> rs)
                            _ -> nglTypeError ("Expected variable "++show var++" to contain a mapped read.")

                        else return []
executeSelectWBlock expr _ _ = unreachable ("Select with block, unexpected argument: " ++ show expr)


interpretArguments :: [(Variable, Expression)] -> InterpretationEnvIO [(T.Text, NGLessObject)]
interpretArguments args =
    forM args $ \(Variable v, e) -> do
        e' <- interpretTopValue e
        return (v, e')

interpretBlock :: [(T.Text, NGLessObject)] -> [Expression] -> InterpretationROEnv BlockResult
interpretBlock vs [] = return (BlockResult BlockOk vs)
interpretBlock vs (e:es) = do
    r <- interpretBlock1 vs e
    case blockStatus r of
        BlockOk -> interpretBlock (blockValues r) es
        _ -> return r

interpretBlock1 :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv BlockResult
interpretBlock1 vs (Optimized (LenThresholdDiscard (Variable v) bop thresh)) = case lookup v vs of
        Just (NGOShortRead r) ->
            let status = if binInt bop (srLength r) thresh
                    then BlockDiscarded
                    else BlockOk in return (BlockResult status vs)
        _ -> throwShouldNotOccur ("Variable name not found in optimized processing " ++ show v)
    where
        binInt :: BOp -> Int -> Int -> Bool
        binInt BOpLT a b = a < b
        binInt BOpGT a b = a > b
        binInt BOpLTE a b = a <= b
        binInt BOpGTE a b = a >= b
        binInt _ _ _ = error "This is impossible: the optimized transformation should ensure this case never exists"
interpretBlock1 vs (Optimized (SubstrimReassign (Variable v) mq)) = case lookup v vs  of
        Just (NGOShortRead r) -> let
                nv = NGOShortRead (substrim mq r)
                vs' = map (\p@(a,_) -> if a == v then (a,nv) else p) vs
            in return (BlockResult BlockOk vs')
        _ -> throwShouldNotOccur ("Variable name not found in optimized processing " ++ show v)
interpretBlock1 vs (Assignment (Variable n) val) = do
    val' <- interpretBlockExpr vs val
    if n `notElem` map fst vs
        then throwShouldNotOccur ("only assignments to block variable are possible [assigning to '"++show n++"']")
        else do
            let vs' = map (\p@(a,_) -> (if a == n then (a,val') else p)) vs
            return $ BlockResult BlockOk vs'
interpretBlock1 vs Discard = return (BlockResult BlockDiscarded vs)
interpretBlock1 vs Continue = return (BlockResult BlockContinued vs)
interpretBlock1 vs (Condition c ifT ifF) = do
    NGOBool v' <- interpretBlockExpr vs c
    interpretBlock1 vs (if v' then ifT else ifF)
interpretBlock1 vs (Sequence expr) = interpretBlock vs expr -- interpret [expr]
interpretBlock1 vs x = unreachable ("interpretBlock1: This should not have happened " ++ show vs ++ " " ++ show x)

interpretBlockExpr :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv NGLessObject
interpretBlockExpr vs val = local (\(NGLInterpretEnv mods e) -> (NGLInterpretEnv mods (Map.union e (Map.fromList vs)))) (interpretPreProcessExpr val)

interpretPreProcessExpr :: Expression -> InterpretationROEnv NGLessObject
interpretPreProcessExpr (FunctionCall (FuncName "substrim") var args _) = do
    NGOShortRead r <- interpretExpr var
    args' <- forM args $ \(Variable v, e) -> do
        e' <- interpretExpr e
        return (v, e')
    mq <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 0) "substrim argument" "min_quality" args'
    return . NGOShortRead $ substrim mq r

interpretPreProcessExpr expr = interpretExpr expr

_evalUnary :: UOp -> NGLessObject -> Either NGError NGLessObject
_evalUnary UOpMinus (NGOInteger n) = return $ NGOInteger (-n)
_evalUnary UOpLen (NGOShortRead r) = return $ NGOInteger . toInteger $ srLength r
_evalUnary UOpNot (NGOBool v) = return $ NGOBool (not v)
_evalUnary op v = nglTypeError ("invalid unary operation ("++show op++") on value " ++ show v)

_evalIndex :: NGLessObject -> [Maybe NGLessObject] -> Either NGError NGLessObject
_evalIndex (NGOList elems) [Just (NGOInteger ix)] = return (elems !! fromInteger ix)
_evalIndex sr index@[Just (NGOInteger a)] = _evalIndex sr $ (Just $ NGOInteger (a + 1)) : index
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Nothing] =
    return . NGOShortRead $ ShortRead rId (B.drop (fromIntegral s) rSeq) (B.drop (fromIntegral s) rQual)
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Nothing, Just (NGOInteger e)] =
    return . NGOShortRead $ ShortRead rId (B.take (fromIntegral e) rSeq) (B.take (fromIntegral e) rQual)

_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Just (NGOInteger e)] = do
    let e' = fromIntegral e
        s' = fromIntegral s
        e'' = e'- s'
    return $ NGOShortRead (ShortRead rId (B.take e'' . B.drop s' $ rSeq) (B.take e'' . B.drop s' $ rQual))
_evalIndex _ _ = nglTypeError ("_evalIndex: invalid operation" :: String)


-- Binary Evaluation
_evalBinary :: BOp ->  NGLessObject -> NGLessObject -> Either NGError NGLessObject
_evalBinary BOpLT (NGOInteger a) (NGOInteger b) = Right $ NGOBool (a < b)
_evalBinary BOpGT (NGOInteger a) (NGOInteger b) = Right $ NGOBool (a > b)
_evalBinary BOpLTE (NGOInteger a) (NGOInteger b) = Right $ NGOBool (a <= b)
_evalBinary BOpGTE (NGOInteger a) (NGOInteger b) = Right $ NGOBool (a >= b)
_evalBinary BOpEQ lexpr rexpr = Right . NGOBool $ lexpr == rexpr
_evalBinary BOpNEQ lexpr rexpr = Right . NGOBool $ lexpr /= rexpr
_evalBinary BOpAdd (NGOInteger a) (NGOInteger b) = Right $ NGOInteger (a + b)
_evalBinary BOpAdd (NGOString a) (NGOString b) = Right $ NGOString (T.concat [a, b])
_evalBinary BOpMul (NGOInteger a) (NGOInteger b) = Right $ NGOInteger (a * b)
_evalBinary op a b = throwScriptError (concat ["_evalBinary: ", show op, " ", show a, " ", show b])

