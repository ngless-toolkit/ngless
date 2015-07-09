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
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
import System.Directory

import Data.String
import Data.Maybe

import Utils.Utils
import Unique
import ProcessFastQ
import Substrim
import Language
import FileManagement
import CountOperation (countAnnotatedSet)
import Configuration (outputDirectory)
import Output

import Interpretation.Annotation
import Interpretation.Write
import Interpretation.Select
import Interpretation.Map
import Interpretation.Reads
import Data.FastQ


{- Interpretation is done inside 3 Monads
 -  1. InterpretationEnvIO
 -      This is the IO Monad with a variable environment
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

-- For now just a string, but should become more descriptive later
data NGError = NGError !T.Text
        deriving (Show, Eq)

instance Error NGError where
    strMsg = NGError . T.pack

instance IsString NGError where
    fromString = NGError . T.pack

throwErrorStr :: (MonadError NGError m) => String -> m a
throwErrorStr = throwError . NGError . T.pack

-- A variable map
type NGLEnv_t = Map.Map T.Text NGLessObject

type InterpretationEnvT m =
            ErrorT
                NGError
                (StateT (Int,NGLEnv_t) m)
-- Monad 1: IO + read-write environment
type InterpretationEnvIO = InterpretationEnvT (ResourceT IO)
-- Monad 2: read-write environment
type InterpretationEnv = InterpretationEnvT Identity
-- Monad 3: read-only environment
type InterpretationROEnv =
            ErrorT
                NGError
                (Reader NGLEnv_t)

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
runInEnv action = do
    env <- gets id
    let Identity (mv,env') = runStateT (runErrorT action) env
    put env'
    case mv of
        Left e -> throwError e
        Right v -> return v

runInROEnv :: InterpretationROEnv a -> InterpretationEnv a
runInROEnv action = do
    (_,env) <- gets id
    let Identity mv = runReaderT (runErrorT action) env
    case mv of
        Left e -> throwError e
        Right v -> return v

runInterpret :: InterpretationROEnv a -> NGLEnv_t -> a
runInterpret action env = case runReaderT (runErrorT action) env of
        Identity (Left _) -> error "Error in interpretation"
        Identity (Right v) -> v

runInROEnvIO :: InterpretationROEnv a -> InterpretationEnvIO a
runInROEnvIO = runInEnv . runInROEnv

-- | By necessity, this code has several unreachable corners
unreachable err = error ("Reached code that was thought to be unreachable!\n"++err)
nglTypeError err = error ("Unexpected type error! This should have been caught by validation!\n"++err)


interpret :: FilePath -> T.Text -> [(Int,Expression)] -> IO ()
interpret fname script es = do
    let nglessScript = NGOString script 
        nglessScriptFname = NGOFilename fname
        initialState = (0, Map.insert ".scriptfname" nglessScriptFname (Map.insert ".script" nglessScript Map.empty))
    odir <- outputDirectory
    setupHtmlViewer odir
    r <- runResourceT $ evalStateT (runErrorT . interpretIO $ es) initialState
    case r of
        Right _ -> outputListLno InfoOutput Nothing ["Ngless finished."]
        Left err -> outputListLno ErrorOutput Nothing [show err]

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO [] = return ()
interpretIO ((ln,e):es) = setlno ln >> interpretTop e >> interpretIO es

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ topFunction f e args b
interpretTop (Condition c ifTrue ifFalse) = do
    NGOBool c' <- runInROEnvIO (interpretExpr c)
    interpretTop (if c'
        then ifTrue
        else ifFalse)
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop _ = throwError "Top level statement is NOP"

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = topFunction f e args b
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup (Variable v)) = lookupVariable v >>= \case
        Nothing -> throwError "Variable lookup error"
        Just r' -> return r'
interpretExpr (BuiltinConstant (Variable "STDIN")) = return (NGOString "/dev/stdin")
interpretExpr (BuiltinConstant (Variable "STDOUT")) = return (NGOString "/dev/stdout")
interpretExpr (BuiltinConstant (Variable v)) = throwErrorStr ("Unknown builtin constant '" ++ show v ++ "': it should not have been accepted.")
interpretExpr (ConstStr t) = return (NGOString t)
interpretExpr (ConstBool b) = return (NGOBool b)
interpretExpr (ConstSymbol s) = return (NGOSymbol s)
interpretExpr (ConstNum n) = return (NGOInteger n)
interpretExpr (UnaryOp op v) = _evalUnary op <$> interpretExpr v
interpretExpr (BinaryOp bop v1 v2) = do
    v1' <- interpretExpr v1
    v2' <- interpretExpr v2
    let r = _evalBinary bop v1' v2'
    return r
interpretExpr (IndexExpression expr ie) = do
    expr' <- interpretExpr expr
    ie' <- interpretIndex ie
    let r = _evalIndex expr' ie'
    return r
interpretExpr (ListExpression e) = NGOList <$> mapM interpretExpr e
interpretExpr not_expr = throwErrorStr ("Expected an expression, received " ++ show not_expr)

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
topFunction Fpreprocess expr _ _ = throwErrorStr ("preprocess expected a variable holding a NGOReadSet, but received: " ++ show expr)
topFunction f expr args block = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    topFunction' f expr' args' block


topFunction' :: FuncName -> NGLessObject -> [(T.Text, NGLessObject)] -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction' Ffastq     expr args Nothing = executeFastq expr args
topFunction' Fsamfile   expr args Nothing = executeSamfile expr args
topFunction' Funique    expr args Nothing = executeUnique expr args
topFunction' Fwrite     expr args Nothing = liftIO (writeToFile expr args)
topFunction' Fmap       expr args Nothing = executeMap expr args
topFunction' Fas_reads  expr args Nothing = liftIO (executeReads expr args)
topFunction' Fselect    expr args Nothing = liftIO (executeSelect expr args)
topFunction' Fannotate  expr args Nothing = executeAnnotation expr args
topFunction' Fcount     expr args Nothing = executeCount expr args
topFunction' Fprint     expr args Nothing = executePrint expr args

topFunction' Fpaired mate1 args Nothing = do
    let Just mate2 = lookup "second" args
        mate3 = lookup "singles" args
    NGOReadSet1 enc1 fp1 <- executeQualityProcess mate1
    NGOReadSet1 enc2 fp2 <- executeQualityProcess mate2
    when (enc1 /= enc2)
        (throwError "Mates do not seem to have the same quality encoding!")
    case mate3 of
        Nothing -> return (NGOReadSet2 enc1 fp1 fp2)
        Just f3 -> do
            NGOReadSet1 enc3 fp3 <- executeQualityProcess f3
            when (enc1 /= enc3)
                (throwError "Mates do not seem to have the same quality encoding!")
            return (NGOReadSet3 enc1 fp1 fp2 fp3)

topFunction' f _ _ _ = throwError . NGError . T.concat $ ["Interpretation of ", T.pack (show f), " is not implemented"]

executeFastq expr args = do
    let NGOSymbol encName = lookupWithDefault (NGOSymbol "auto") "encoding" args
        enc = case encName of
                "auto" -> Nothing
                "33" -> Just SangerEncoding
                "sanger" -> Just SangerEncoding
                "64" -> Just SolexaEncoding
                "solexa" -> Just SolexaEncoding
                _ -> error "impossible to reach"

    case expr of
        (NGOString fname) -> executeQualityProcess' enc (T.unpack fname) "beforeQC"
        (NGOList fps) -> NGOList <$> sequence [executeQualityProcess' enc (T.unpack fname) "beforeQC" | NGOString fname <- fps]
        v -> throwErrorStr ("fastq function: unexpected first argument: " ++ show v)

executeSamfile expr [] = do
    case expr of
        (NGOString fname) -> return $ NGOMappedReadSet (T.unpack fname) Nothing
        (NGOList sams) -> NGOList <$> sequence [executeSamfile s [] | s <- sams]
        v -> throwErrorStr ("samfile function: unexpected first argument: " ++ show v)
executeSamfile _ args = throwErrorStr ("samfile does not take any arguments, got " ++ show args)

executeCount :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (\x -> executeCount x args) e
executeCount (NGOAnnotatedSet p) args = do
    let c = lookup "counts" args
        NGOInteger m = lookupWithDefault (NGOInteger 0) "min" args
    res <- liftIO $ countAnnotatedSet p c m
    return $ NGOAnnotatedSet res

executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

executeAnnotation :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeAnnotation (NGOList e) args = NGOList <$> mapM (\x -> executeAnnotation x args) e
executeAnnotation (NGOMappedReadSet e dDS) args = do
    let g = T.unpack . evalString <$> lookup "gff" args
        m = parseAnnotationMode $ lookup "mode" args
        a = fromMaybe False $ evalBool <$> lookup "ambiguity" args
        s = fromMaybe False $ evalBool <$> lookup "strand" args
        fs = lookup "features" args >>= Just <$> \case
            (NGOSymbol f) -> [T.unpack f]
            (NGOList feats') -> map (T.unpack . evalSymbol) feats'
            _ -> unreachable "executeAnnotation: TYPE ERROR"
    res <- liftIO $ annotate e g fs dDS m a s
    return $ NGOAnnotatedSet res
executeAnnotation e _ = throwErrorStr ("Annotation can handle MappedReadSet(s) only. Got " ++ show e)

parseAnnotationMode Nothing = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "union")) = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "intersection_strict")) = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "intersection_non_empty")) = IntersectNonEmpty
parseAnnotationMode m = error (concat ["Unexpected annotation mode (", show m, "). Please submit a bug report."])


executeQualityProcess :: NGLessObject -> InterpretationEnvIO NGLessObject
executeQualityProcess (NGOList e) = NGOList <$> mapM executeQualityProcess e
executeQualityProcess (NGOReadSet1 enc fname) = executeQualityProcess' (Just enc) fname "afterQC"
executeQualityProcess (NGOReadSet2 enc fp1 fp2) = do
    NGOReadSet1 enc1 fp1' <- executeQualityProcess' (Just enc) fp1 "afterQC"
    NGOReadSet1 enc2 fp2' <- executeQualityProcess' (Just enc) fp2 "afterQC"
    when (enc1 /= enc2)
        (throwError "Mates do not seem to have the same quality encoding!")
    return (NGOReadSet2 enc1 fp1' fp2')
executeQualityProcess (NGOReadSet3 enc fp1 fp2 fp3) = do
    NGOReadSet1 enc1 fp1' <- executeQualityProcess' (Just enc) fp1 "afterQC"
    NGOReadSet1 enc2 fp2' <- executeQualityProcess' (Just enc) fp2 "afterQC"
    NGOReadSet1 enc3 fp3' <- executeQualityProcess' (Just enc) fp3 "afterQC"
    when (enc1 /= enc2 || enc2 /= enc3)
        (throwError "Mates do not seem to have the same quality encoding!")
    return (NGOReadSet3 enc1 fp1' fp2' fp3')
executeQualityProcess (NGOString fname) = do
    let fname' = T.unpack fname
    executeQualityProcess' Nothing fname' "beforeQC"

executeQualityProcess v = throwErrorStr ("QC expected a string or readset. Got " ++ show v)
executeQualityProcess' enc fname info = liftIO $ executeQProc enc fname info

executeMap :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeMap fps args = case lookup "reference" args of
    Nothing       -> error "A reference must be suplied"
    Just (NGOString ref) -> executeMap' fps
        where
            executeMap' (NGOList es) = NGOList <$> forM es executeMap'
            executeMap' (NGOReadSet1 _enc file)    = liftResourceT $ interpretMapOp ref file
            executeMap' (NGOReadSet2 _enc fp1 fp2) = liftResourceT $ interpretMapOp2 ref fp1 fp2
            executeMap' v = throwErrorStr ("map of " ++ show v ++ " not implemented yet")
    _         -> error "map could not parse reference argument"

executeUnique :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeUnique (NGOList e) args = NGOList <$> mapM (\x -> executeUnique x args) e
executeUnique (NGOReadSet1 enc file) args = do
        d <- liftIO $
            readReadSet enc file 
                        >>= writeToNFiles file enc
        let NGOInteger mc = lookupWithDefault (NGOInteger 1) "max_copies" args
        uniqueCalculations' mc d --default
    where
        uniqueCalculations' :: Integer -> FilePath -> InterpretationEnvIO NGLessObject
        uniqueCalculations' numMaxOccur d = do
            nFp <- liftIO $
                readNFiles enc (fromIntegral numMaxOccur) d >>= \x -> writeReadSet file x enc
            return $ NGOReadSet1 enc nFp

executeUnique expr _ = throwErrorStr ("executeUnique: Cannot handle argument " ++ show expr)


executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block = NGOList <$> mapM (\x -> executePreprocess x args _block) e
executePreprocess (NGOReadSet1 enc file) _args (Block [Variable var] block) = do
        liftIO $ outputListLno' DebugOutput ["Preprocess on ", file]
        rs <- map NGOShortRead <$> liftIO (readReadSet enc file)
        env <- gets snd
        newfp <- liftIO $ writeReadSet file (execBlock env rs) enc
        return $ NGOReadSet1 enc newfp
    where
        execBlock env = mapMaybe (\r -> runInterpret (interpretPBlock1 block var r) env)
executePreprocess (NGOReadSet2 enc fp1 fp2) _args block = executePreprocess (NGOReadSet3 enc fp1 fp2 "") _args block
executePreprocess (NGOReadSet3 enc fp1 fp2 fp3) _args (Block [Variable var] block) = do
        liftIO $ outputListLno' DebugOutput (["Preprocess on paired end ",
                                                fp1, "+", fp2] ++ (if fp3 /= ""
                                                                    then [" with singles ", fp3]
                                                                    else []))
        (fp1', out1) <- liftIO $ openNGLTempFile fp1 "preprocessed.1." ".fq"
        (fp2', out2) <- liftIO $ openNGLTempFile fp2 "preprocessed.2." ".fq"
        (fps, hs) <- liftIO $ openNGLTempFile fp1 "preprocessed.singles." ".fq"
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
        liftIO $ outputLno' DebugOutput "Preprocess finished"
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
        intercalate _ _ _ _ = throwError "preprocess: paired mates do not contain the same number of reads"

        writeSR h sr = liftIO $ BL.hPut h (asFastQ enc [sr])
executePreprocess v _ _ = error ("executePreprocess: Cannot handle this input: " ++ show v)


interpretPBlock1 :: Expression -> T.Text -> NGLessObject -> InterpretationROEnv (Maybe ShortRead)
interpretPBlock1 block var r = do
    r' <- interpretBlock1 [(var, r)] block
    case blockStatus r' of
        BlockDiscarded -> return Nothing -- Discard Read.
        _ -> case lookup var (blockValues r') of
                Just (NGOShortRead rr) -> case srLength rr of
                    0 -> return Nothing
                    _ -> return (Just rr)
                _ -> unreachable ("Expected variable "++show var++" to contain a short read.")

executePrint :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executePrint (NGOString s) [] = liftIO (T.putStr s) >> return NGOVoid
executePrint err  _ = throwErrorStr ("Cannot print " ++ show err)

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
        then throwError "only assignments to block variable are possible"
        else do
            let vs' = map (\p@(a,_) -> (if a == n then (a,val') else p)) vs
            return $ BlockResult BlockOk vs'
interpretBlock1 vs Discard = return (BlockResult BlockDiscarded vs)
interpretBlock1 vs Continue = return (BlockResult BlockContinued vs)
interpretBlock1 vs (Condition c ifT ifF) = do
    NGOBool v' <- interpretBlockExpr vs c
    interpretBlock1 vs (if v' then ifT else ifF)
interpretBlock1 vs (Sequence expr) = interpretBlock vs expr -- interpret [expr]
interpretBlock1 vs x = error ("interpretBlock1: This should not have happened " ++ show vs ++ " " ++ show x)

interpretBlockExpr :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv NGLessObject
interpretBlockExpr vs val = local (\e -> Map.union e (Map.fromList vs)) (interpretPreProcessExpr val)

interpretPreProcessExpr :: Expression -> InterpretationROEnv NGLessObject
interpretPreProcessExpr (FunctionCall Fsubstrim var args _) = do
    NGOShortRead r <- interpretExpr var
    args' <- interpretArguments args
    let mq = fromMaybe 0 $ (fromIntegral . evalInteger) <$> lookup "min_quality" args'
    return . NGOShortRead $ substrim mq r

interpretPreProcessExpr expr = interpretExpr expr

_evalUnary :: UOp -> NGLessObject -> NGLessObject
_evalUnary UOpMinus (NGOInteger n) = NGOInteger (-n)
_evalUnary UOpLen (NGOShortRead r) = NGOInteger . toInteger $ srLength r
_evalUnary op v = nglTypeError ("invalid unary operation ("++show op++") on value " ++ show v)

_evalIndex :: NGLessObject -> [Maybe NGLessObject] -> NGLessObject
_evalIndex sr index@[Just (NGOInteger a)] = _evalIndex sr $ (Just $ NGOInteger (a + 1)) : index
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Nothing] =
    NGOShortRead $ ShortRead rId (B.drop (fromIntegral s) rSeq) (B.drop (fromIntegral s) rQual)
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Nothing, Just (NGOInteger e)] =
    NGOShortRead $ ShortRead rId (B.take (fromIntegral e) rSeq) (B.take (fromIntegral e) rQual)

_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Just (NGOInteger e)] = do
    let e' = fromIntegral e
        s' = fromIntegral s
        e'' = e'- s'
    NGOShortRead (ShortRead rId (B.take e'' . B.drop s' $ rSeq) (B.take e'' . B.drop s' $ rQual))
_evalIndex _ _ = nglTypeError "_evalIndex: invalid operation"

evalBool (NGOBool x) = x
evalBool _ = nglTypeError "evalBool: Argument type must be NGOBool"

evalString (NGOString s) = s
evalString o = nglTypeError ("evalString: Argument type must be NGOString (received " ++ show o ++ ").")

evalSymbol (NGOSymbol s) = s
evalSymbol o = nglTypeError ("evalSymbol: Argument type must be NGOSymbol (received " ++ show o ++ ").")

evalInteger (NGOInteger i) = i
evalInteger o = nglTypeError ("evalInteger: Argument type must be NGOInteger (got " ++ show o ++ ").")


-- Binary Evaluation
_evalBinary :: BOp ->  NGLessObject -> NGLessObject -> NGLessObject
_evalBinary BOpLT (NGOInteger a) (NGOInteger b) = NGOBool (a < b)
_evalBinary BOpGT (NGOInteger a) (NGOInteger b) = NGOBool (a > b)
_evalBinary BOpLTE (NGOInteger a) (NGOInteger b) = NGOBool (a <= b)
_evalBinary BOpGTE (NGOInteger a) (NGOInteger b) = NGOBool (a >= b)
_evalBinary BOpEQ lexpr rexpr =  NGOBool $ lexpr == rexpr
_evalBinary BOpNEQ lexpr rexpr =  NGOBool $ lexpr /= rexpr
_evalBinary BOpAdd (NGOInteger a) (NGOInteger b) = NGOInteger (a + b)
_evalBinary BOpMul (NGOInteger a) (NGOInteger b) = NGOInteger (a * b)
_evalBinary op a b = nglTypeError (concat ["_evalBinary: ", show op, " ", show a, " ", show b])


getScriptName = do
    -- This cannot fail as we inserted the variable ourselves
    Just (NGOFilename fname) <- runInROEnvIO $ lookupVariable ".scriptfname"
    return fname
