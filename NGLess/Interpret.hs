{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpret
    ( interpret
    , _evalIndex
    , _evalLen
    , _evalBinary
    , _evalMinus
    ) where


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.String
import Data.Maybe

import Unique
import ProcessFastQ
import Substrim
import Language
import FileManagement
import JSONManager
import CountOperation
import Configuration (outputDirectory)
import Output

import Interpretation.Annotation
import Interpretation.Write
import Interpretation.Map
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
interpretIO ((ln,e):es) = (setlno ln >> interpretTop e >> interpretIO es)

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = interpretTopValue val >>= setVariableValue var
interpretTop (FunctionCall f e args b) = void $ topFunction f e args b
interpretTop (Condition c ifTrue ifFalse) = do
    c' <- runInROEnvIO (interpretExpr c)
    if evalBool c'
        then interpretTop ifTrue
        else interpretTop ifFalse
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop _ = throwError "Top level statement is NOP"

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue (FunctionCall f e args b) = topFunction f e args b
interpretTopValue e = runInROEnvIO (interpretExpr e)

interpretExpr :: Expression -> InterpretationROEnv NGLessObject
interpretExpr (Lookup (Variable v)) = do
    r <- lookupVariable v
    case r of
        Nothing -> throwError "Variable lookup error"
        Just r' -> return r'
interpretExpr (ConstStr t) = return (NGOString t)
interpretExpr (ConstBool b) = return (NGOBool b)
interpretExpr (ConstSymbol s) = return (NGOSymbol s)
interpretExpr (ConstNum n) = return (NGOInteger n)
interpretExpr (UnaryOp op v) = evalUOP op <$> interpretExpr v
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
interpretExpr _ = throwError "Not an expression"


interpretIndex :: Index -> InterpretationROEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: (Maybe Expression) -> InterpretationROEnv (Maybe NGLessObject)
maybeInterpretExpr Nothing = return Nothing
maybeInterpretExpr (Just e) = Just <$> interpretExpr e

topFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction Ffastq expr _args _block = do
    expr' <- interpretTopValue expr
    executeQualityProcess expr'

topFunction Funique expr args _block = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    executeUnique expr' args'

topFunction Fpreprocess expr@(Lookup (Variable varName)) args (Just _block) = do
    expr' <- runInROEnvIO $ interpretExpr expr
    args' <- runInROEnvIO $ interpretArguments args
    res' <- executePreprocess expr' args' _block varName >>= executeQualityProcess 
    setVariableValue varName res'
    return res'

topFunction Fpreprocess expr _ _ = error ("Should be used a variable with a NGOReadSet, but is: " ++ (show expr))

topFunction Fwrite expr args _ = do 
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    liftIO (writeToFile expr' args')

topFunction Fmap expr args _ = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    executeMap expr' args'

topFunction Fannotate expr args _ = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    executeAnnotation expr' args'

topFunction Fcount expr args _ = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    executeCount expr' args'

topFunction f _ _ _ = throwError . NGError . T.concat $ ["Interpretation of ", T.pack (show f), " is not implemented"]

executeCount :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (\x -> executeCount x args) e
executeCount (NGOAnnotatedSet p) args = do
    let c = lookup "counts" args
        m = fromMaybe (NGOInteger 0) $ lookup "min" args
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
        features = lookup "features" args
        fs = case features of
            Nothing -> Nothing
            Just (NGOSymbol f) -> Just [T.unpack f]
            Just (NGOList feats') -> Just $ (map (T.unpack . evalSymbol)) $ feats'
            Just _ -> unreachable "executeAnnotation: TYPE ERROR"
    res <- liftIO $ annotate e g fs dDS m a s
    return $ NGOAnnotatedSet res
executeAnnotation e _ = error ("Invalid Type. Should be used NGOList or NGOMappedReadSet but type was: " ++ (show e))

parseAnnotationMode Nothing = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "union")) = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "intersection_strict")) = IntersectUnion
parseAnnotationMode (Just (NGOSymbol "intersection_non_empty")) = IntersectNonEmpty
parseAnnotationMode m = error (concat ["Unexpected annotation mode (", show m, "). Please submit a bug report."])


executeQualityProcess :: NGLessObject -> InterpretationEnvIO NGLessObject
executeQualityProcess (NGOList e) = NGOList <$> mapM executeQualityProcess e
executeQualityProcess (NGOReadSet fname enc nt) = executeQualityProcess' (Just enc) fname "afterQC" (B.unpack nt)
executeQualityProcess (NGOString fname) = do
    let fname' = T.unpack fname
    r <- getScriptName
    newTemplate <- liftIO $ generateDirId fname' -- new template only calculated once.
    _ <- liftIO $ insertFilesProcessedJson newTemplate (T.pack r)
    executeQualityProcess' Nothing fname' "beforeQC" newTemplate

executeQualityProcess _ = throwError("Should be passed a ConstStr or [ConstStr]")
executeQualityProcess' enc fname info nt = liftIO $ executeQProc enc fname info nt

executeMap :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeMap (NGOList es) args = do
    res <- forM es $ \e ->
                executeMap e args
    return (NGOList res)
executeMap (NGOReadSet file _enc _) args = case lookup "reference" args of 
    Just refPath' -> liftResourceT $ interpretMapOp (evalString refPath') file
    Nothing       -> error "A reference must be suplied"

executeMap _ _ = error ("Not implemented yet")

executeUnique :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeUnique (NGOList e) args = return . NGOList =<< mapM (\x -> executeUnique x args) e
executeUnique (NGOReadSet file enc t) args = do
        d <- liftIO $ 
            readReadSet enc file 
                        >>= writeToNFiles file enc
        case lookup "max_copies" args of
            Just v -> uniqueCalculations' (v' v) d
            _      -> uniqueCalculations' 1 d --default
    where 
        v' = fromIntegral . evalInteger
        uniqueCalculations' :: Int -> FilePath -> InterpretationEnvIO NGLessObject
        uniqueCalculations' numMaxOccur' d = do
            nFp <- liftIO $ 
                readNFiles enc numMaxOccur' d >>= \x -> writeReadSet file x enc 
            return $ NGOReadSet nFp enc t

executeUnique _ _ = error "executeUnique: Should not have happened"


executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> T.Text -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block v = return . NGOList =<< mapM (\x -> executePreprocess x args _block v) e
executePreprocess (NGOReadSet file enc t) args (Block [Variable var] expr) _ = do
        liftIO $ outputListLno' DebugOutput ["Preprocess on ", file]
        rs <- map NGOShortRead <$> liftIO (readReadSet enc file)
        env <- gets snd
        newfp <- liftIO $ writeReadSet file (map asShortRead (execBlock env rs)) enc
        return $ NGOReadSet newfp enc t
    where
        execBlock env = mapMaybe (\r -> runInterpret (interpretPBlock1 r) env)
        interpretPBlock1 :: NGLessObject -> InterpretationROEnv (Maybe NGLessObject)
        interpretPBlock1 r = do
            r' <- interpretBlock1 ((var, r) : args) expr
            case blockStatus r' of
                BlockDiscarded -> return Nothing -- Discard Read.
                _ -> do
                    let newRead = lookup var (blockValues r')
                    case newRead of
                        Just value -> case evalInteger $ _evalLen value of
                            0 -> return Nothing
                            _ -> return newRead
                        _ -> throwError "A read should have been returned."
             
executePreprocess a _ _ _ = error ("executePreprocess: This should have not happened." ++ show a)

interpretArguments :: [(Variable, Expression)] -> InterpretationROEnv [(T.Text, NGLessObject)]
interpretArguments = mapM interpretArguments'
    where interpretArguments' ((Variable v), e) = do
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
    if not (n `elem` (map fst vs))
        then error "only assignments to block variable are possible"
        else do
            let vs' = map (\p@(a,_) -> (if a == n then (a,val') else p)) vs
            return $ BlockResult BlockOk vs'
interpretBlock1 vs Discard = return (BlockResult BlockDiscarded vs)
interpretBlock1 vs Continue = return (BlockResult BlockContinued vs)
interpretBlock1 vs (Condition c ifT ifF) = do
    v' <- interpretBlockExpr vs c
    interpretBlock1 vs (if evalBool v' then ifT else ifF)
interpretBlock1 vs (Sequence expr) = interpretBlock vs expr -- interpret [expr]
interpretBlock1 vs x = error ("interpretBlock1: This should not have happened " ++ show vs ++ " " ++ show x)

interpretBlockExpr :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv NGLessObject
interpretBlockExpr vs val = local (\e -> Map.union e (Map.fromList vs)) (interpretPreProcessExpr val)

interpretPreProcessExpr :: Expression -> InterpretationROEnv NGLessObject
interpretPreProcessExpr (FunctionCall Fsubstrim var args _) = do
    expr' <- interpretExpr var
    args' <- interpretArguments args
    return . NGOShortRead $ substrim (getvalue args') (asShortRead expr')
    where
        getvalue els = fromIntegral . evalInteger $ fromMaybe (NGOInteger 0) (lookup "min_quality" els)

interpretPreProcessExpr expr = interpretExpr expr

evalUOP :: UOp -> NGLessObject -> NGLessObject
evalUOP UOpMinus x@(NGOInteger _) = _evalMinus x
evalUOP UOpLen sr@(NGOShortRead _) = _evalLen sr
evalUOP _ _ = nglTypeError "invalid unary operation. "

_evalLen (NGOShortRead r) = NGOInteger . toInteger $ srLength r
_evalLen err = nglTypeError ("Length must receive a Read. Received a " ++ show err)

_evalMinus (NGOInteger n) = NGOInteger (-n)
_evalMinus err = nglTypeError ("Minus operator must receive a integer. Received a" ++ show err)

_evalIndex :: NGLessObject -> [Maybe NGLessObject] -> NGLessObject
_evalIndex sr index@[Just (NGOInteger a)] = _evalIndex sr $ (Just $ NGOInteger (a + 1)) : index
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Nothing] =
    NGOShortRead $ ShortRead rId (B.drop (fromIntegral s) rSeq) (B.drop (fromIntegral s) rQual)
_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Nothing, Just (NGOInteger e)] =
    NGOShortRead $ ShortRead rId (B.take (fromIntegral e) rSeq) (B.take (fromIntegral e) rQual)

_evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Just (NGOInteger e)] = do
    let e' = (fromIntegral e)
        s' = (fromIntegral s)
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


asShortRead (NGOShortRead r) = r
asShortRead _ = nglTypeError "Short read expected"

getScriptName = do
    -- This cannot fail as we inserted the variable ourselves
    Just (NGOFilename fname) <- runInROEnvIO $ lookupVariable ".scriptfname"
    return fname
