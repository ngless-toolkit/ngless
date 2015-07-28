{- Copyright 2013-2015 NGLess Authors
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

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($=), ($$), (=$=))

import System.IO
import System.Directory
import Data.Maybe

import Utils.Utils
import ProcessFastQ
import Substrim
import Language
import FileManagement
import Configuration (outputDirectory)
import Output
import NGLess
import Data.Sam
import Data.FastQ

import Interpretation.Annotation
import Interpretation.Write
import Interpretation.Select
import Interpretation.Map
import Interpretation.Reads
import Interpretation.Count
import Unique


{- Interpretation is done inside 3 Monads
 -  1. InterpretationEnvIO
 -      This is the NGLessIO monad with a variable environment on top
 -  2. InterpretationEnv
 -      This is the read-write variable environment
 -  3. InterpretationROEnv
 -      This is a read-only variable environment.
 - 
 - Monad (1) is a superset of (2) which is a superset of (3). runInEnv and
 - friends switch between the monads.
 -
 -
 - For blocks, we have a special system where block-variables are read-write,
 - but others are read-only.
 -
 - Functions inside the interpret monads are named interpret*, helper
 - non-monadic functions which perform computations are named eval*.
 -
 -}


-- A variable map
type NGLEnv_t = Map.Map T.Text NGLessObject

type InterpretationEnvT m =
                (StateT (Int,NGLEnv_t) m)
-- Monad 1: IO + read-write environment
type InterpretationEnvIO = InterpretationEnvT NGLessIO
-- Monad 2: read-write environment
type InterpretationEnv = InterpretationEnvT (ExceptT NGError Identity)
-- Monad 3: read-only environment
type InterpretationROEnv = ExceptT NGError (Reader NGLEnv_t)

{- The result of a block is a status indicating why the block finished
 - and the value of all block variables.
 -}
data BlockStatus = BlockOk | BlockDiscarded | BlockContinued
    deriving (Eq,Show)
data BlockResult = BlockResult
                { blockStatus :: BlockStatus
                , blockValues :: [(T.Text, NGLessObject)]
                } deriving (Eq,Show)

-- Set line number
setlno :: Int -> InterpretationEnvIO ()
setlno !n = do
    modify $ \(_,e) -> (n,e)
    liftIO $ setOutputLno (Just n)

lookupVariable :: T.Text -> InterpretationROEnv (Maybe NGLessObject)
lookupVariable !k =
    Map.lookup k `fmap` ask

setVariableValue :: T.Text -> NGLessObject -> InterpretationEnvIO ()
setVariableValue !k !v = modify $ \(n,e) -> (n, Map.insert k v e)

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
    (_,env) <- gets id
    let Identity mv = runReaderT (runExceptT action) env
    case mv of
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
unreachable err = throwShouldNotOccurr ("Reached code that was thought to be unreachable!\n"++err)
nglTypeError err = throwShouldNotOccurr ("Unexpected type error! This should have been caught by validation!\n"++err)


traceExpr m e =
    runNGLessIO $ outputListLno' TraceOutput ["Interpreting [", m , "]: ", show e]

interpret :: FilePath -> T.Text -> [(Int,Expression)] -> NGLessIO ()
interpret fname script es = do
    let nglessScript = NGOString script 
        nglessScriptFname = NGOFilename fname
        initialState = (0, Map.insert ".scriptfname" nglessScriptFname (Map.insert ".script" nglessScript Map.empty))
    odir <- outputDirectory
    liftIO $ setupHtmlViewer odir
    evalStateT (interpretIO $ es) initialState
    outputListLno InfoOutput Nothing ["Ngless finished."]

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO [] = return ()
interpretIO ((ln,e):es) = setlno ln >> traceExpr "interpretIO" e >> interpretTop e >> interpretIO es

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = traceExpr "assignment" val >> interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ topFunction f e args b
interpretTop (Condition c ifTrue ifFalse) = do
    NGOBool c' <- runInROEnvIO (interpretExpr c)
    interpretTop (if c'
        then ifTrue
        else ifFalse)
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop _ = throwShouldNotOccurr ("Top level statement is NOP" :: String)

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = topFunction f e args b
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup (Variable v)) = lookupVariable v >>= \case
        Nothing -> throwScriptError ("Variable lookup error" :: String)
        Just r' -> return r'
interpretExpr (BuiltinConstant (Variable "STDIN")) = return (NGOString "/dev/stdin")
interpretExpr (BuiltinConstant (Variable "STDOUT")) = return (NGOString "/dev/stdout")
interpretExpr (BuiltinConstant (Variable v)) = throwShouldNotOccurr ("Unknown builtin constant '" ++ show v ++ "': it should not have been accepted.")
interpretExpr (ConstStr t) = return (NGOString t)
interpretExpr (ConstBool b) = return (NGOBool b)
interpretExpr (ConstSymbol s) = return (NGOSymbol s)
interpretExpr (ConstNum n) = return (NGOInteger n)
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
    arg' <- maybeInterpretExpr arg
    args' <- interpretArguments args
    executeMethod met self' arg' args'
interpretExpr not_expr = throwShouldNotOccurr ("Expected an expression, received " ++ show not_expr)

interpretIndex :: Index -> InterpretationROEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: Maybe Expression -> InterpretationROEnv (Maybe NGLessObject)
maybeInterpretExpr Nothing = return Nothing
maybeInterpretExpr (Just e) = Just <$> interpretExpr e

topFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction Fpreprocess expr@(Lookup (Variable varName)) args (Just block) = do
    expr' <- runInROEnvIO $ interpretExpr expr
    args' <- runInROEnvIO $ interpretArguments args
    res' <- executePreprocess expr' args' block >>= executeQualityProcess
    setVariableValue varName res'
    return res'
topFunction Fpreprocess expr _ _ = throwShouldNotOccurr ("preprocess expected a variable holding a NGOReadSet, but received: " ++ show expr)
topFunction f expr args block = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    topFunction' f expr' args' block

topFunction' :: FuncName -> NGLessObject -> KwArgsValues -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction' Ffastq     expr args Nothing = executeFastq expr args
topFunction' Fsamfile   expr args Nothing = executeSamfile expr args
topFunction' Funique    expr args Nothing = runNGLessIO (executeUnique expr args)
topFunction' Fwrite     expr args Nothing = traceExpr "write" expr >> runNGLessIO (executeWrite expr args)
topFunction' Fmap       expr args Nothing = executeMap expr args
topFunction' Fas_reads  expr args Nothing = runNGLessIO (executeReads expr args)
topFunction' Fselect    expr args Nothing = runNGLessIO (executeSelect expr args)
topFunction' Fannotate  expr args Nothing = runNGLessIO (executeAnnotation expr args)
topFunction' Fcount     expr args Nothing = runNGLessIO (executeCount expr args)
topFunction' Fprint     expr args Nothing = executePrint expr args

topFunction' Fpaired mate1 args Nothing = do
    let Just mate2 = lookup "second" args
        mate3 = lookup "singles" args
    NGOReadSet1 enc1 fp1 <- executeQualityProcess mate1
    NGOReadSet1 enc2 fp2 <- executeQualityProcess mate2
    when (enc1 /= enc2) $
        throwDataError ("Mates do not seem to have the same quality encoding!" :: String)
    case mate3 of
        Nothing -> return (NGOReadSet2 enc1 fp1 fp2)
        Just f3 -> do
            NGOReadSet1 enc3 fp3 <- executeQualityProcess f3
            when (enc1 /= enc3) $
                throwDataError ("Mates do not seem to have the same quality encoding!" :: String)
            return (NGOReadSet3 enc1 fp1 fp2 fp3)
topFunction' Fselect expr args (Just b) = executeSelectWBlock expr args b
topFunction' f _ _ _ = throwShouldNotOccurr . concat $ ["Interpretation of ", (show f), " is not implemented"]

executeFastq expr args = do
    traceExpr "fastq" expr
    let NGOSymbol encName = lookupWithDefault (NGOSymbol "auto") "encoding" args
    enc <- case encName of
            "auto" -> return Nothing
            "33" -> return $ Just SangerEncoding
            "sanger" -> return $ Just SangerEncoding
            "64" -> return $ Just SolexaEncoding
            "solexa" -> return $ Just SolexaEncoding
            _ -> unreachable "impossible to reach"
    case expr of
        (NGOString fname) -> executeQualityProcess' enc (T.unpack fname) "beforeQC"
        (NGOList fps) -> NGOList <$> sequence [executeQualityProcess' enc (T.unpack fname) "beforeQC" | NGOString fname <- fps]
        v -> unreachable ("fastq function: unexpected first argument: " ++ show v)

executeSamfile expr [] = do
    traceExpr "samfile" expr
    case expr of
        (NGOString fname) -> return $ NGOMappedReadSet (T.unpack fname) Nothing
        (NGOList sams) -> NGOList <$> sequence [executeSamfile s [] | s <- sams]
        v -> unreachable ("samfile function: unexpected first argument: " ++ show v)
executeSamfile _ args = unreachable ("samfile does not take any arguments, got " ++ show args)


executeQualityProcess :: NGLessObject -> InterpretationEnvIO NGLessObject
executeQualityProcess (NGOList e) = NGOList <$> mapM executeQualityProcess e
executeQualityProcess (NGOReadSet1 enc fname) = executeQualityProcess' (Just enc) fname "afterQC"
executeQualityProcess (NGOReadSet2 enc fp1 fp2) = do
    NGOReadSet1 enc1 fp1' <- executeQualityProcess' (Just enc) fp1 "afterQC"
    NGOReadSet1 enc2 fp2' <- executeQualityProcess' (Just enc) fp2 "afterQC"
    when (enc1 /= enc2) $
        throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ")")
    return (NGOReadSet2 enc1 fp1' fp2')
executeQualityProcess (NGOReadSet3 enc fp1 fp2 fp3) = do
    NGOReadSet1 enc1 fp1' <- executeQualityProcess' (Just enc) fp1 "afterQC"
    NGOReadSet1 enc2 fp2' <- executeQualityProcess' (Just enc) fp2 "afterQC"
    NGOReadSet1 enc3 fp3' <- executeQualityProcess' (Just enc) fp3 "afterQC"
    when (enc1 /= enc2 || enc2 /= enc3) $
        throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ", third one is " ++ show enc3 ++")")
    return (NGOReadSet3 enc1 fp1' fp2' fp3')
executeQualityProcess (NGOString fname) = do
    let fname' = T.unpack fname
    executeQualityProcess' Nothing fname' "beforeQC"

executeQualityProcess v = nglTypeError ("QC expected a string or readset. Got " ++ show v)
executeQualityProcess' enc fname info = runNGLessIO $ executeQProc enc fname info

executeMap :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeMap fps args = case lookup "reference" args of
    Nothing  -> throwScriptError ("A reference must be suplied" ::String)
    Just (NGOString ref) -> executeMap' fps
        where
            executeMap' (NGOList es) = NGOList <$> forM es executeMap'
            executeMap' (NGOReadSet1 _enc file)    = runNGLessIO $ interpretMapOp ref file
            executeMap' (NGOReadSet2 _enc fp1 fp2) = runNGLessIO $ interpretMapOp2 ref fp1 fp2
            executeMap' v = throwShouldNotOccurr ("map of " ++ show v ++ " not implemented yet")
    _         -> unreachable "map could not parse reference argument"


executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block = NGOList <$> mapM (\x -> executePreprocess x args _block) e
executePreprocess (NGOReadSet1 enc file) _args (Block [Variable var] block) = do
        runNGLessIO $ outputListLno' DebugOutput ["Preprocess on ", file]
        rs <- map NGOShortRead <$> liftIO (readReadSet enc file)
        (newfp, h) <- runNGLessIO $ openNGLTempFile file "preprocessed_" "fq.gz"
        C.yieldMany rs
                $= C.mapM transformInterpret
                =$= C.filter isJust
                =$= C.map (toStrict . asFastQ enc . (:[]) . fromJust)
                =$= C.gzip
                $$ C.sinkHandle h
        liftIO (hClose h)
        return $ NGOReadSet1 enc newfp
    where
        toStrict = B.concat . BL.toChunks
        transformInterpret :: NGLessObject -> InterpretationEnvIO (Maybe ShortRead)
        transformInterpret r = runInROEnvIO $ interpretPBlock1 block var r
executePreprocess (NGOReadSet2 enc fp1 fp2) _args block = executePreprocess (NGOReadSet3 enc fp1 fp2 "") _args block
executePreprocess (NGOReadSet3 enc fp1 fp2 fp3) _args (Block [Variable var] block) = do
        runNGLessIO $ outputListLno' DebugOutput (["Preprocess on paired end ",
                                                fp1, "+", fp2] ++ (if fp3 /= ""
                                                                    then [" with singles ", fp3]
                                                                    else []))
        (fp1', out1) <- runNGLessIO $ openNGLTempFile fp1 "preprocessed.1." ".fq"
        (fp2', out2) <- runNGLessIO $ openNGLTempFile fp2 "preprocessed.2." ".fq"
        (fps, hs) <- runNGLessIO $ openNGLTempFile fp1 "preprocessed.singles." ".fq"
        rs1 <- map NGOShortRead <$> liftIO (readReadSet enc fp1)
        rs2 <- map NGOShortRead <$> liftIO (readReadSet enc fp2)
        anySingle <- intercalate (out1, out2, hs) rs1 rs2 False
        rs3 <- (if fp3 /= ""
                    then map NGOShortRead <$> liftIO (readReadSet enc fp3)
                    else return [])
        anySingle' <- preprocBlock hs rs3
        liftIO $ hClose `mapM_` [out1, out2, hs]
        unless (anySingle || anySingle')
            (liftIO $ removeFile fps)
        runNGLessIO $ outputLno' DebugOutput "Preprocess finished"
        return $ if anySingle || anySingle'
                    then NGOReadSet3 enc fp1' fp2' fps
                    else NGOReadSet2 enc fp1' fp2'
    where
        preprocBlock :: Handle -> [NGLessObject] -> InterpretationEnvIO Bool
        preprocBlock hsingles rs = do
            ps <- forM rs  (runInROEnvIO . interpretPBlock1 block var)
            case catMaybes ps of
                [] -> return False
                ps' -> do
                    forM_ ps' (liftIO . writeSR hsingles)
                    return True
        intercalate _ [] [] anySingle = return anySingle
        intercalate hs@(out1, out2, hsingles) (r1:rs1) (r2:rs2) !anySingle = do
            r1' <- runInROEnvIO $ interpretPBlock1 block var r1
            r2' <- runInROEnvIO $ interpretPBlock1 block var r2
            anySingle' <- case (r1',r2') of
                (Nothing, Nothing) -> return anySingle
                (Just r1'', Just r2'') -> do
                    writeSR out1 r1''
                    writeSR out2 r2''
                    return anySingle
                (Just r, Nothing) -> writeSR hsingles r >> return True
                (Nothing, Just r) -> writeSR hsingles r >> return True
            intercalate hs rs1 rs2 anySingle'
        intercalate _ _ _ _ = throwDataError ("preprocess: paired mates do not contain the same number of reads" :: String)

        writeSR h sr = liftIO $ BL.hPut h (asFastQ enc [sr])
executePreprocess v _ _ = unreachable ("executePreprocess: Cannot handle this input: " ++ show v)

executeMethod :: MethodName -> NGLessObject -> Maybe NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationROEnv NGLessObject
executeMethod Mflag (NGOMappedRead samline) (Just (NGOSymbol flag)) [] = case getFlag flag of
        Left err -> throwError err
        Right f -> return (NGOBool $ f samline)
    where
        getFlag "mapped" = Right isAligned
        getFlag "unmapped" = Right $ not . isAligned
        getFlag ferror = throwScriptError ("Flag " ++ show ferror ++ " is unknown for method flag")
executeMethod m self arg kwargs = error ("Method " ++ show m ++ " with self="++show self ++ " arg="++ show arg ++ " kwargs="++show kwargs ++ " is not implemented")


interpretPBlock1 :: Expression -> T.Text -> NGLessObject -> InterpretationROEnv (Maybe ShortRead)
interpretPBlock1 block var r = do
    r' <- interpretBlock1 [(var, r)] block
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
executeSelectWBlock (NGOMappedReadSet fname ref) [] (Block [Variable var] body) = do
        runNGLessIO $ outputListLno' TraceOutput ["Executing blocked select on file ", fname]
        let oname = fname ++ ".selected"
        C.sourceFile fname
            $= CB.lines
            =$= C.filterM filterLine
            =$= C.unlinesAscii
            $$ C.sinkFile oname
        return (NGOMappedReadSet oname ref)
    where
        filterLine line
            | "@" `B.isPrefixOf` line = return True -- The whole header is copied verbatim to the output
            | otherwise = do
                    let mr = NGOMappedRead (readSamLine . BL.fromChunks $ [line])
                    mr' <- runInROEnvIO (interpretBlock1 [(var, mr)] body)
                    return (blockStatus mr' `elem` [BlockContinued, BlockOk])
executeSelectWBlock _ _ _ = unreachable ("Select with block")


interpretArguments :: [(Variable, Expression)] -> InterpretationROEnv [(T.Text, NGLessObject)]
interpretArguments = mapM interpretArguments'
    where interpretArguments' (Variable v, e) = do
            e' <- interpretExpr e
            return (v,e')

interpretBlock :: [(T.Text, NGLessObject)] -> [Expression] -> InterpretationROEnv BlockResult
interpretBlock vs [] = return (BlockResult BlockOk vs)
interpretBlock vs (e:es) = do
    r <- interpretBlock1 vs e 
    case blockStatus r of
        BlockOk -> interpretBlock (blockValues r) es
        _ -> return r

interpretBlock1 :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv BlockResult
interpretBlock1 vs (Assignment (Variable n) val) = do
    val' <- interpretBlockExpr vs val
    if n `notElem` (map fst vs)
        then throwShouldNotOccurr ("only assignments to block variable are possible [assigning to '"++show n++"']")
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
interpretBlockExpr vs val = local (\e -> Map.union e (Map.fromList vs)) (interpretPreProcessExpr val)

interpretPreProcessExpr :: Expression -> InterpretationROEnv NGLessObject
interpretPreProcessExpr (FunctionCall Fsubstrim var args _) = do
    NGOShortRead r <- interpretExpr var
    args' <- interpretArguments args
    let mq = lookupWithDefault (NGOInteger 0) "min_quality" args'
    mq' <- getInt mq
    return . NGOShortRead $ substrim mq' r

interpretPreProcessExpr expr = interpretExpr expr

getInt (NGOInteger i) = return (fromInteger i)
getInt o = nglTypeError ("getInt: Argument type must be NGOInteger (got " ++ show o ++ ").")


_evalUnary :: UOp -> NGLessObject -> Either NGError NGLessObject
_evalUnary UOpMinus (NGOInteger n) = return $ NGOInteger (-n)
_evalUnary UOpLen (NGOShortRead r) = return $ NGOInteger . toInteger $ srLength r
_evalUnary UOpNot (NGOBool v) = return $ NGOBool (not v)
_evalUnary op v = nglTypeError ("invalid unary operation ("++show op++") on value " ++ show v)

_evalIndex :: NGLessObject -> [Maybe NGLessObject] -> Either NGError NGLessObject
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
_evalBinary BOpMul (NGOInteger a) (NGOInteger b) = Right $ NGOInteger (a * b)
_evalBinary op a b = throwScriptError (concat ["_evalBinary: ", show op, " ", show a, " ", show b])

