{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP #-}
module Interpret
    ( interpret
#ifdef IS_BUILDING_TEST
    , _evalIndex
    , _evalUnary
    , evalBinary
#endif
    ) where

{-| This is the interpreter module. The main function, 'interpet' expects an
 - AST and executes it.
 -
 - Most types in ngless refer to files on disk or streams and not to data (as
 - the data elements are quite large).
 -
 - Interpretation is fairly trivial pattern matching, with the exception that
 - interpreting blocks uses heavily tuned code.
 -
 - Ngless functions are implemented by Haskell functions with the signature
 - 'NGLessObject -> KwArgsValues -> NGLessIO NGLessObject' (the first argument
 - is the unnamed argument, followed by a list of keyword arguments). By
 - convention, they are named 'execute*' (this is a convention only).
 -
 - Several builtin functions are implemented by the files in the
 - Interpretation/ directory.
 -
 - # Interpretation
 -
 -  Interpretation is done inside two monads
 -  1. InterpretationEnvIO
 -      This is the NGLessIO monad with a variable environment on top
 -  2. InterpretationROEnv
 -      This is a read-only variable environment on top of NGLess
 -
 - For blocks, we have a special system where block-variables are read-write,
 - but others are read-only.
 -
 - Functions inside the interpret monads are named interpret*, helper
 - non-monadic functions which perform computations are named eval*.
 -
 - # TODO/Improvement ideas
 -
 - Replacing the variable map with a vector could potentially be faster (only
 - matters for block interpretation).
 -}


import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Algorithms.Utils as CAlg
import qualified Data.Conduit.Algorithms.Async as CAlg
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ
import qualified Data.Conduit.TQueue as CA
import           Control.Monad.Extra (whenJust)
import           Control.Concurrent.STM (atomically)
import           Control.DeepSeq (NFData(..))
import qualified Data.Vector as V
import           Safe (atMay)
import           Control.Error (note)
import           Text.Read (readEither)

import System.IO
import System.Directory
import Data.List (find)
import GHC.Conc                 (getNumCapabilities)

import Language
import FileManagement
import FileOrStream
import Output
import Modules
import NGLess
import Data.Sam
import Data.FastQ
import NGLess.NGLEnvironment
import NGLess.NGError

import Interpretation.Map
import Interpretation.Count (executeCount, executeCountCheck)
import Interpretation.CountFile (executeCountFile)
import Interpretation.FastQ
import Interpretation.Write
import Interpretation.Select (executeSelect, executeMappedReadMethod, reinjectSequences)
import Interpretation.Unique
import Interpretation.Substrim
import Utils.Utils
import Utils.Suggestion
import Utils.Conduit


type SimpleVariableMap = Map.Map T.Text NGLessObject
data VariableMap = VariableMapGlobal SimpleVariableMap
                    | VariableMapBlock BlockVariables SimpleVariableMap

variableMapLookup v (VariableMapGlobal sm) = Map.lookup v sm
variableMapLookup v (VariableMapBlock b sm) = lookupBlockVar v b <|> Map.lookup v sm

data NGLInterpretEnv = NGLInterpretEnv
    { ieModules :: [Module]
    , ieVariableEnv :: VariableMap
    }

-- Monad 1: IO + read-write environment
type InterpretationEnvIO = StateT  NGLInterpretEnv NGLessIO
-- Monad 2: pure read-only environment
type InterpretationROEnv = ReaderT NGLInterpretEnv NGLess

runInterpretationRO :: NGLInterpretEnv -> InterpretationROEnv a -> NGLess a
runInterpretationRO env act = runReaderT act env

runNGLessIO :: NGLessIO a -> InterpretationEnvIO a
runNGLessIO = lift

liftNGLessIO :: NGLessIO a -> InterpretationEnvIO a
liftNGLessIO = lift

runInROEnvIO :: InterpretationROEnv a -> InterpretationEnvIO a
runInROEnvIO act = do
    env <- get
    runNGLess $ runReaderT act env

{-| The result of a block is a status indicating why the block finished
 - and the value of all block variables.
 -}
data BlockStatus = BlockOk | BlockDiscarded | BlockContinued
    deriving (Eq,Show)

data BlockVariables = BlockVariables1 !T.Text NGLessObject
            deriving (Eq, Show)

data BlockResult = BlockResult
                { blockStatus :: {-# UNPACK #-} !BlockStatus
                , blockValues :: BlockVariables
                } deriving (Eq,Show)

autoComprehendNB f (NGOList es) args = NGOList <$> sequence [f e args | e <- es]
autoComprehendNB f e args = f e args

-- Set line number
setlno :: Int -> InterpretationEnvIO ()
setlno !n = runNGLessIO $ updateNglEnvironment (\e -> e { ngleLno = Just n } )

lookupVariable :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupVariable !k = liftM2 (<|>)
    (lookupConstant k)
    (asks (variableMapLookup k . ieVariableEnv))

lookupConstant :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupConstant !k = do
    constants <- asks (concatMap modConstants . ieModules)
    case filter ((==k) . fst) constants of
        [] -> return Nothing
        [(_,v)] -> return (Just v)
        _ -> throwShouldNotOccur ("Multiple hits found for constant " ++ T.unpack k)


setVariableValue :: T.Text -> NGLessObject -> InterpretationEnvIO ()
setVariableValue !k !v = modify $ \case
                            (NGLInterpretEnv mods (VariableMapGlobal vm)) -> (NGLInterpretEnv mods (VariableMapGlobal (Map.insert k v vm)))
                            _ -> error "This should never happen (setVariableValue)"

findFunction :: FuncName -> InterpretationEnvIO (NGLessObject -> KwArgsValues -> NGLessIO NGLessObject)
findFunction fname@(FuncName fname') = do
        mods <- gets ieModules
        case filter hasF mods of
            [m] -> case find ((== fname) . funcName) (modFunctions m) of
                    Just func -> do
                        let wrap = if funcAllowsAutoComprehension func
                                        then autoComprehendNB
                                        else id
                        return $ wrap $ (runFunction m) fname'
                    _ ->  throwShouldNotOccur . T.unpack $ T.concat ["Function '", fname', "' not found (not builtin and not in any loaded module), even though it should have."]
            [] -> throwShouldNotOccur . T.unpack $ T.concat ["Function '", fname', "' not found (not builtin and not in any loaded module)"]
            ms -> throwShouldNotOccur . T.unpack $ T.concat (["Function '", T.pack $ show fname, "' found in multiple modules! ("] ++ [T.concat [modname, ":"] | modname <- modName . modInfo <$> ms])
    where
        hasF m = fname `elem` (funcName `fmap` modFunctions m)


-- | By necessity, this code has several unreachable corners

unreachable :: (MonadError NGError m) => String -> m a
unreachable err = throwShouldNotOccur ("Reached code that was thought to be unreachable!\n"++err)
nglTypeError err = throwShouldNotOccur ("Unexpected type error! This should have been caught by validation!\n"++err)

traceExpr m e =
    runNGLessIO $ outputListLno' TraceOutput ["Interpreting [", m , "]: ", show e]

interpret :: [Module] -> [(Int,Expression)] -> NGLessIO ()
interpret modules es = do
    evalStateT (interpretIO es) (NGLInterpretEnv modules $ VariableMapGlobal Map.empty)
    outputListLno InfoOutput Nothing ["Interpretation finished."]

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO es = forM_ es $ \(ln,e) -> do
    setlno ln
    gcTemps
    traceExpr "interpretIO" e
    interpretTop e

gcTemps :: InterpretationEnvIO ()
gcTemps = do
    active <- gets $ \case
                NGLInterpretEnv _ (VariableMapGlobal t) -> Map.elems t
                _ -> error "gcTemps in BLOCK?!"
    let extractFiles = \case
                            NGOString _ -> []
                            NGOBool _ -> []
                            NGOInteger _ -> []
                            NGODouble _ -> []
                            NGOSymbol _ -> []
                            NGOFilename f -> [f]
                            NGOShortRead _ -> []
                            NGOReadSet _ rs -> extractFilesRS rs
                            NGOSequenceSet s -> origin s
                            NGOMappedReadSet _ s _ -> origin s
                            NGOMappedRead _ -> []
                            NGOCounts s -> origin s
                            NGOVoid -> []
                            NGOList lst -> concatMap extractFiles lst
        extractFilesRS (ReadSet ps p3) = concatMap (extractFilesRS' . fst) ps
                                                ++ concatMap (extractFilesRS' . snd) ps
                                                ++ concatMap extractFilesRS' p3
        extractFilesRS' (FastQFilePath _ f) = [f]
        activeFiles = concatMap extractFiles active
    runNGLessIO $ do
        outputListLno' DebugOutput ["Running garbage collection."]
        createdFiles <- ngleTemporaryFilesCreated <$> nglEnvironment
        let garbage = filter (`notElem` activeFiles) createdFiles
        -- Use removeIfTemporary because it respects command line flags &c
        forM_ garbage removeIfTemporary

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = traceExpr "assignment" val >> interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ interpretFunction f e args b
interpretTop (Condition c ifTrue ifFalse) = do
    c' <- runInROEnvIO (interpretExpr c >>= boolOrTypeError "interpreting if condition")
    interpretTop (if c'
        then ifTrue
        else ifFalse)
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop e = throwShouldNotOccur ("Unexpected top level statement: "++show e)

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = interpretFunction f e args b
interpretTopValue (ListExpression es) = NGOList <$> mapM interpretTopValue es
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup _ (Variable v)) = lookupVariable v >>= \case
        Nothing -> throwScriptError ("Could not lookup variable `"++show v++"`")
        Just r' -> return r'
interpretExpr (BuiltinConstant (Variable "STDIN")) = return (NGOString "/dev/stdin")
interpretExpr (BuiltinConstant (Variable "STDOUT")) = return (NGOString "/dev/stdout")
interpretExpr (BuiltinConstant (Variable "__VOID")) = return NGOVoid
interpretExpr (BuiltinConstant (Variable v)) = throwShouldNotOccur ("Unknown builtin constant '" ++ show v ++ "': it should not have been accepted.")
interpretExpr (ConstStr t) = return (NGOString t)
interpretExpr (ConstBool b) = return (NGOBool b)
interpretExpr (ConstSymbol s) = return (NGOSymbol s)
interpretExpr (ConstInt n) = return (NGOInteger n)
interpretExpr (ConstDouble n) = return (NGODouble n)
interpretExpr (UnaryOp op v) = do
    v' <- interpretExpr v
    runNGLess (_evalUnary op v')
interpretExpr (BinaryOp bop v1 v2) = do
    v1' <- interpretExpr v1
    v2' <- interpretExpr v2
    runNGLess (evalBinary bop v1' v2')
interpretExpr (IndexExpression expr ie) = do
    expr' <- interpretExpr expr
    ie' <- interpretIndex ie
    runNGLess (_evalIndex expr' ie')
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
interpretFunction (FuncName "preprocess") expr args (Just block) = do
    expr' <- runInROEnvIO $ interpretExpr expr
    args' <- interpretArguments args
    executePreprocess expr' args' block
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
interpretFunction' (FuncName "__merge_samfiles")  expr args Nothing = runNGLessIO (executeMergeSams expr args)
interpretFunction' (FuncName "select")    expr args Nothing = runNGLessIO (executeSelect expr args)
interpretFunction' (FuncName "count")     expr args Nothing = runNGLessIO (executeCount expr args)
interpretFunction' (FuncName "__check_count") expr args Nothing = runNGLessIO (executeCountCheck expr args)
interpretFunction' (FuncName "countfile") expr args Nothing = runNGLessIO (executeCountFile expr args)
interpretFunction' (FuncName "print")     expr args Nothing = executePrint expr args
interpretFunction' (FuncName "read_int")  expr args Nothing = executeReadInt expr args
interpretFunction' (FuncName "read_double")  expr args Nothing = executeReadDouble expr args
interpretFunction' (FuncName "paired")   mate1 args Nothing = runNGLessIO (executePaired mate1 args)
interpretFunction' (FuncName "select")    expr args (Just b) = executeSelectWBlock expr args b
interpretFunction' (FuncName "__assert")  expr [] args       = executeAssert expr args
interpretFunction' fname@(FuncName fname') expr args Nothing = do
    traceExpr ("executing module function: '"++T.unpack fname'++"'") expr
    execF <- findFunction fname
    runNGLessIO (execF expr args)
interpretFunction' f _ _ _ = throwShouldNotOccur . concat $ ["Interpretation of ", show f, " is not implemented"]

executeSamfile expr@(NGOString fname) args = do
    traceExpr "samfile" expr
    gname <- lookupStringOrScriptErrorDef (return fname) "samfile group name" "name" args
    headers <- lookupStringOrScriptErrorDef (return "") "samfile headers" "headers" args
    let headers' = T.unpack headers
        fname' = T.unpack fname
    let checkf f = do
            err <- liftIO (checkFileReadable f)
            whenJust err (throwDataError . T.unpack)
    checkf fname'
    if null headers'
        then return $ NGOMappedReadSet gname (File fname') Nothing
        else do
            checkf headers'
            let out = Stream
                            [File fname', File headers']
                            fname'
                            ((CB.sourceFile headers' >> CB.sourceFile fname') .| linesVC 4096)
            return $ NGOMappedReadSet gname out Nothing
executeSamfile e args = unreachable ("executeSamfile " ++ show e ++ " " ++ show args)

data PreprocessPairOutput = Paired !ShortRead !ShortRead | Single !ShortRead
instance NFData PreprocessPairOutput where
    rnf (Paired _ _) = ()
    rnf (Single _) = ()

splitPreprocessPair :: V.Vector PreprocessPairOutput -> (V.Vector ShortRead, V.Vector ShortRead, V.Vector ShortRead)
splitPreprocessPair input = (V.mapMaybe extract1 input, V.mapMaybe extract2 input, V.mapMaybe extractS input)
    where
        extract1 = \case
            Paired sr _ -> Just sr
            _ -> Nothing
        extract2 = \case
            Paired _ sr -> Just sr
            _ -> Nothing
        extractS = \case
            Single sr -> Just sr
            _ -> Nothing


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

shortReadVectorStats :: (MonadIO m, MonadResource m) => m (TQ.TBMQueue (V.Vector ShortRead), ReleaseKey, A.Async FQStatistics)
shortReadVectorStats = do
    q <- liftIO $ TQ.newTBMQueueIO 8
    p <- liftIO . A.async $
        C.runConduit $ CA.sourceTBMQueue q
            .| CC.concat
            .| fqStatsC
    k <- register (atomically . TQ.closeTBMQueue $ q)
    return (q, k, p)

writeAndContinue :: MonadIO m => TQ.TBMQueue (V.Vector ShortRead) -> C.ConduitT (V.Vector ShortRead) (V.Vector ShortRead) m ()
writeAndContinue q = CL.iterM (liftIO . atomically . TQ.writeTBMQueue q)

executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block = NGOList <$> mapM (\x -> executePreprocess x args _block) e
executePreprocess (NGOReadSet name (ReadSet pairs singles)) args (Block (Variable var) block) = do
        -- This is a bit complex, but preprocess was slow at first and we try
        -- to take full advantage of parallelism.
        --
        -- Pipeline is
        --    [read file *] -> [decode in blocks] -> [pre QC *] -> [preproc*] -> [post QC*] -> [encode & write *]
        -- where the starred (*) elements are done in a separate thread
        -- for the QC threads, this is done by using a queue & writing data there
        env <- gets id
        liftNGLessIO $ do
            keepSingles <- lookupBoolOrScriptErrorDef (return True) "preprocess argument" "keep_singles" args
            qcInput <- lookupBoolOrScriptErrorDef (return False) "preprocess" "__input_qc" args
            numCapabilities <- liftIO getNumCapabilities
            let mapthreads = 4 * max 1 (numCapabilities - 2)

            [(q1, k1, s1), (q2, k2, s2), (q3, k3, s3)] <- replicateM 3 shortReadVectorStats


            let inencs = fqpathEncoding <$> (fst <$> pairs) ++ (snd <$> pairs) ++ singles
                outenc
                    | allSame inencs = head inencs
                    | otherwise = SangerEncoding
            let asSource :: [FastQFilePath] -> C.ConduitT () (V.Vector ShortRead) NGLessIO ()
                asSource [] = return ()
                asSource (FastQFilePath enc f:rest) =
                        let input = conduitPossiblyCompressedFile f
                                .| linesVC 4096
                                .| CAlg.enumerateC
                                .| CAlg.asyncMapEitherC mapthreads (\(!i,v) -> fqDecodeVector (4096*i) enc v)
                        in do
                            if not qcInput
                                then input
                                else do
                                    (q, k, s) <- lift shortReadVectorStats
                                    input .| writeAndContinue q
                                    lift $ release k
                                    s' <- liftIO $ A.wait s
                                    lift $ outputFQStatistics f s' enc
                            asSource rest


                write nt h q =
                        writeAndContinue q
                            .| CAlg.asyncMapC nt (B.concat . map (fqEncode outenc) . V.toList)
                            .| CAlg.asyncZstdTo 3 h

            let processpairs :: (V.Vector ShortRead, V.Vector ShortRead) -> NGLess (V.Vector ShortRead, V.Vector ShortRead, V.Vector ShortRead)
                processpairs = fmap splitPreprocessPair . vMapMaybeLifted (runInterpretationRO env . intercalate keepSingles) . uncurry V.zip
            (fp1', out1) <- openNGLTempFile "" "preprocessed.1." "fq.zst"
            (fp2', out2) <- openNGLTempFile "" "preprocessed.2." "fq.zst"
            (fp3', out3) <- openNGLTempFile "" "preprocessed.singles." "fq.zst"

            C.runConduit $
                zipSource2 (asSource (fst <$> pairs)) (asSource (snd <$> pairs))
                    .| CAlg.asyncMapEitherC mapthreads processpairs
                    .| (void $ C.sequenceSinks
                        [CL.map (\(a,_,_) -> a) .| write mapthreads out1 q1
                        ,CL.map (\(_,a,_) -> a) .| write mapthreads out2 q2
                        ,CL.map (\(_,_,a) -> a) .| write mapthreads out3 q3
                        ])

            C.runConduit $
                asSource singles
                    .| CAlg.asyncMapEitherC mapthreads (vMapMaybeLifted (runInterpretationRO env . interpretPBlock1 block var))
                    .| void (write mapthreads out3 q3)

            forM_ [k1, k2, k3] release
            liftIO $ forM_ [out1, out2, out3] hClose
            [s1',s2',s3'] <- forM [s1,s2,s3] (liftIO . A.wait)

            outputListLno' DebugOutput ["Preprocess finished"]

            Just lno <- ngleLno <$> nglEnvironment
            outputFQStatistics ("preproc.lno"++show lno++".pairs.1") s1' outenc
            outputFQStatistics ("preproc.lno"++show lno++".pairs.2") s2' outenc
            outputFQStatistics ("preproc.lno"++show lno++".singles") s3' outenc
            NGOReadSet name <$> case (nSeq s1' > 0, nSeq s2' > 0, nSeq s3' > 0) of
                        (True, True, False) -> do
                            liftIO $ removeFile fp3'
                            return $ ReadSet [(FastQFilePath outenc fp1', FastQFilePath outenc fp2')] []
                        (False, False, True) -> do
                            forM_ [fp1', fp2'] (liftIO . removeFile)
                            return $ ReadSet [] [FastQFilePath outenc fp3']
                        _
                            | null pairs -> do
                                forM_ [fp1', fp2'] (liftIO . removeFile)
                                return $ ReadSet [] [FastQFilePath outenc fp3']
                            | otherwise -> return $ ReadSet [(FastQFilePath outenc fp1', FastQFilePath outenc fp2')] [FastQFilePath outenc fp3']
    where
        intercalate :: Bool -> (ShortRead, ShortRead) -> InterpretationROEnv (Maybe PreprocessPairOutput)
        intercalate keepSingles (r1, r2) = do
                r1' <- interpretPBlock1 block var r1
                r2' <- interpretPBlock1 block var r2
                return $ case (r1',r2') of
                    (Just r1'', Just r2'') -> Just (Paired r1'' r2'')
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
executeMethod method (NGOShortRead sr) arg kwargs = runNGLess (executeShortReadsMethod method sr arg kwargs)
executeMethod (MethodName "to_string") (NGODouble val) _ _  = return . NGOString . T.pack . show $ val
executeMethod (MethodName "to_string") (NGOInteger val) _ _ = return . NGOString . T.pack . show $ val
executeMethod m self arg kwargs = throwShouldNotOccur ("Method " ++ show m ++ " with self="++show self ++ " arg="++ show arg ++ " kwargs="++show kwargs ++ " is not implemented")


interpretPBlock1 :: Expression -> T.Text -> ShortRead -> InterpretationROEnv (Maybe ShortRead)
interpretPBlock1 block var r = do
    r' <- interpretBlock1 (BlockVariables1 var (NGOShortRead r)) block
    case blockStatus r' of
        BlockDiscarded -> return Nothing -- Discard Read.
        _ -> case lookupBlockVar var (blockValues r') of
                Just (NGOShortRead rr) -> case srLength rr of
                    0 -> return Nothing
                    _ -> return (Just rr)
                _ -> nglTypeError ("Expected variable "++show var++" to contain a short read.")

executePrint :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executePrint (NGOString s) [] = liftIO (T.putStr s) >> return NGOVoid
executePrint err  _ = throwScriptError ("Cannot print " ++ show err)

executeReadInt :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeReadInt (NGOString "") kwargs = NGOInteger <$> lookupIntegerOrScriptError "read_int" "on_empty_return" kwargs
executeReadInt (NGOString s) _ = case readEither (T.unpack s) of
    Right val -> return $! NGOInteger val
    Left err -> throwDataError ("Could not parse integer from '"++T.unpack s++"'. Error: "++err)
executeReadInt s _ = throwScriptError ("Cannot parse this object as integer: "++ show s)

executeReadDouble :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeReadDouble (NGOString "") kwargs = NGODouble <$> lookupDoubleOrScriptError "read_int" "on_empty_return" kwargs
executeReadDouble (NGOString s) _ = case readEither (T.unpack s) of
    Right val -> return $! NGODouble val
    Left err -> throwDataError ("Could not parse double from '"++T.unpack s++"'. Error: "++err)
executeReadDouble s _ = throwScriptError ("Cannot parse this object as double: "++ show s)

executeAssert (NGOBool True) _ = return NGOVoid
executeAssert (NGOBool False) _ = throwScriptError "Assert failed"
executeAssert _ _ = throwShouldNotOccur "Assert did not receive a boolean!"

executeSelectWBlock :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executeSelectWBlock input@NGOMappedReadSet{ nglSamFile = isam} args (Block (Variable var) body) = do
        paired <- lookupBoolOrScriptErrorDef (return True) "select" "paired" args
        outputHeader <- lookupBoolOrScriptErrorDef (return True) "select" "__output_header" args
        let (samfp, istream) = asSamStream isam
        runNGLessIO $ outputListLno' TraceOutput ["Executing blocked select on file ", samfp]
        env <- gets id
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        -- See "Notes on 'Sequence reinjection'" in NGLess/Environment/Select.hs
        doReinject <- runNGLessIO $ do
                            v <- ngleVersion <$> nglEnvironment
                            if v < NGLVersion 0 8
                                then do
                                    outputListLno' WarningOutput ["Select changed behaviour (for the better) in ngless 0.8. If possible, upgrade your version statement."]
                                    return False
                                else return True
        oname <- runNGLessIO $ makeNGLTempFile samfp "block_selected_" "sam.zstd" $ \ohandle ->
                C.runConduit $
                    istream
                        .| do
                            when outputHeader $
                                CC.takeWhileE (isSamHeaderString . unwrapByteLine)
                                .| CL.map concatBytelines
                            readSamGroupsC' mapthreads paired
                                .| CAlg.asyncMapEitherC mapthreads (fmap concatLines . V.mapM (runInterpretationRO env . selectBlock doReinject))
                        .|  CAlg.asyncZstdTo 3 ohandle
        return input { nglSamFile = File oname }
    where
        concatBytelines :: V.Vector ByteLine -> B.ByteString
        concatBytelines =  concatLines' . map (BB.byteString . unwrapByteLine) . V.toList

        concatLines :: V.Vector [BB.Builder] -> B.ByteString
        concatLines = concatLines' . concat . V.toList

        concatLines' :: [BB.Builder] -> B.ByteString
        concatLines' = BL.toStrict . BB.toLazyByteString . mconcat . map (`mappend` BB.char7 '\n')

        selectBlock :: Bool -> [SamLine] -> InterpretationROEnv [BB.Builder]
        selectBlock _ [] = return []
        selectBlock _ [SamHeader line] = return [BB.byteString line]
        selectBlock doReinject mappedreads  = do
                    mrs' <- interpretBlock1 (BlockVariables1 var (NGOMappedRead mappedreads)) body
                    if blockStatus mrs' `elem` [BlockContinued, BlockOk]
                        then case lookupBlockVar var (blockValues mrs') of
                            Just (NGOMappedRead []) -> return []
                            Just (NGOMappedRead rs) -> do
                                rs' <- runNGLess $ reinjectSequences mappedreads rs
                                return (encodeSamLine <$> (if doReinject then rs' else rs))
                            _ -> nglTypeError ("Expected variable "++show var++" to contain a mapped read.")

                        else return []
executeSelectWBlock expr _ _ = unreachable ("Select with block, unexpected argument: " ++ show expr)


interpretArguments :: [(Variable, Expression)] -> InterpretationEnvIO [(T.Text, NGLessObject)]
interpretArguments args =
    forM args $ \(Variable v, e) -> do
        e' <- interpretTopValue e
        return (v, e')

lookupBlockVar :: T.Text -> BlockVariables -> Maybe NGLessObject
lookupBlockVar n' (BlockVariables1 n v)
    | n == n' = Just v
    | otherwise = Nothing

updateBlockVar (BlockVariables1 n _) n' v'
    | n /= n' = throwShouldNotOccur ("only assignments to block variable are possible [assigning to '"++show n'++"']")
    | otherwise = return $! BlockVariables1 n v'

interpretBlock :: BlockVariables -> [Expression] -> InterpretationROEnv BlockResult
interpretBlock vs [] = return (BlockResult BlockOk vs)
interpretBlock vs (e:es) = do
    r <- interpretBlock1 vs e
    case blockStatus r of
        BlockOk -> interpretBlock (blockValues r) es
        _ -> return r

interpretBlock1 :: BlockVariables -> Expression -> InterpretationROEnv BlockResult
interpretBlock1 vs (Optimized (LenThresholdDiscard (Variable v) bop thresh)) = case lookupBlockVar v vs of
        Just (NGOShortRead r) -> do
            let status = if binInt bop (srLength r) thresh
                    then BlockDiscarded
                    else BlockOk
            return (BlockResult status vs)
        _ -> throwShouldNotOccur ("Variable name not found in optimized processing " ++ show v)
    where
        binInt :: BOp -> Int -> Int -> Bool
        binInt BOpLT a b = a < b
        binInt BOpGT a b = a > b
        binInt BOpLTE a b = a <= b
        binInt BOpGTE a b = a >= b
        binInt _ _ _ = error "This is impossible: the optimized transformation should ensure this case never exists"
interpretBlock1 vs (Optimized (SubstrimReassign (Variable v) mq)) = case lookupBlockVar v vs  of
        Just (NGOShortRead r) -> do
            let nv = NGOShortRead $! substrim mq r
            vs' <- updateBlockVar vs v nv
            return $! BlockResult BlockOk vs'
        _ -> throwShouldNotOccur ("Variable name not found in optimized processing " ++ show v)
interpretBlock1 vs (Assignment (Variable n) val) = do
    val' <- interpretBlockExpr vs val
    vs' <- updateBlockVar vs n val'
    return $ BlockResult BlockOk vs'
interpretBlock1 vs Discard = return (BlockResult BlockDiscarded vs)
interpretBlock1 vs Continue = return (BlockResult BlockContinued vs)
interpretBlock1 vs (Condition c ifT ifF) = do
    v' <- interpretBlockExpr vs c >>= \case
                    NGOBool c' -> return c'
                    _ -> throwShouldNotOccur "Wrong type for condition (Interpret.hs:interpretBlock1)"
    interpretBlock1 vs (if v' then ifT else ifF)
interpretBlock1 vs (Sequence expr) = interpretBlock vs expr -- interpret [expr]
interpretBlock1 vs x = unreachable ("interpretBlock1: This should not have happened " ++ show vs ++ " " ++ show x)

interpretBlockExpr :: BlockVariables -> Expression -> InterpretationROEnv NGLessObject
interpretBlockExpr vs val = local (\(NGLInterpretEnv mods (VariableMapGlobal sm)) -> (NGLInterpretEnv mods (VariableMapBlock vs sm))) (interpretPreProcessExpr val)

interpretPreProcessExpr :: Expression -> InterpretationROEnv NGLessObject
interpretPreProcessExpr (FunctionCall (FuncName "substrim") var args _) = do
    r <- interpretExpr var >>= \case
                        NGOShortRead r -> return r
                        _ -> throwShouldNotOccur "Wrong type in Interpret.hs:interpretExpr"
    args' <- forM args $ \(Variable v, e) -> do
        e' <- interpretExpr e
        return (v, e')
    mq <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 0) "substrim argument" "min_quality" args'
    return . NGOShortRead $ substrim mq r
interpretPreProcessExpr (FunctionCall (FuncName "endstrim") var args _) = do
    r <- interpretExpr var >>= \case
                        NGOShortRead r -> return r
                        _ -> throwShouldNotOccur "Wrong type in Interpret.hs:interpretExpr"
    args' <- forM args $ \(Variable v, e) -> do
        e' <- interpretExpr e
        return (v, e')
    mq <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 0) "endstrim argument" "min_quality" args'
    ends <- lookupSymbolOrScriptErrorDef (return "both") "endstrim argument" "from_ends" args'
    ends' <- case ends of
        "both" -> return EndstrimBoth
        "3" -> return Endstrim3
        "5" -> return Endstrim5
        other -> throwScriptError ("Illegal argument for `from_ends`: "++show other)
    return . NGOShortRead $ endstrim ends' mq r
interpretPreProcessExpr (FunctionCall (FuncName "smoothtrim") var args _) = do
    r <- interpretExpr var >>= \case
                        NGOShortRead r -> return r
                        _ -> throwShouldNotOccur "Wrong type in Interpret.hs:interpretExpr"
    args' <- forM args $ \(Variable v, e) -> do
        e' <- interpretExpr e
        return (v, e')
    mq <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 0) "smoothtrim argument" "min_quality" args'
    w <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 0) "smoothtrim argument" "window" args'
    return . NGOShortRead $ smoothtrim w mq r

interpretPreProcessExpr expr = interpretExpr expr

_evalUnary :: UOp -> NGLessObject -> Either NGError NGLessObject
_evalUnary UOpMinus (NGOInteger n) = return $ NGOInteger (-n)
_evalUnary UOpLen (NGOShortRead r) = return $ NGOInteger . toInteger $ srLength r
_evalUnary UOpLen (NGOList elems) = return $ NGOInteger . toInteger $ length elems
_evalUnary UOpNot (NGOBool v) = return $ NGOBool (not v)
_evalUnary op v = nglTypeError ("invalid unary operation ("++show op++") on value " ++ show v)

_evalIndex :: NGLessObject -> [Maybe NGLessObject] -> Either NGError NGLessObject
_evalIndex (NGOList elems) [Just (NGOInteger ix)] = note (NGError ScriptError errmsg) $ atMay elems (fromInteger ix)
    where errmsg = "Accessing element "++show ix ++ " in list of size "++show (length elems) ++ "."
_evalIndex sr index@[Just (NGOInteger a)] = _evalIndex sr $ Just (NGOInteger (a + 1)) : index
_evalIndex (NGOShortRead sr) [Just (NGOInteger s), Nothing] = let s' = fromInteger s in
    return . NGOShortRead $ srSlice s' (srLength sr - s') sr
_evalIndex (NGOShortRead sr) [Nothing, Just (NGOInteger e)] =
    return . NGOShortRead $ srSlice 0 (fromInteger e) sr
_evalIndex (NGOShortRead sr) [Just (NGOInteger s), Just (NGOInteger e)] =
    return . NGOShortRead $ srSlice (fromInteger s) (fromInteger $ e - s) sr
_evalIndex _ _ = nglTypeError ("_evalIndex: invalid operation" :: String)

