{- Copyright 2013-2014 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpret
    ( interpret,
     interpretBlock,
     evalIndex,
     evalLen,
     evalBinary,
     evalMinus,
     executeUnique,
     executeQualityProcess,
     executePreprocess,
     executeMap,
     executeAnnotation
    ) where


import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

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
import Annotation
import CountOperation
import Configuration

import System.Directory(removeDirectoryRecursive)

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
type InterpretationEnvIO = InterpretationEnvT IO
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
setlno !n = modify $ \(_,e) -> (n,e)

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

interpret :: FilePath -> T.Text -> [(Int,Expression)] -> IO ()
interpret fname script es = do
    let nglessScript = NGOString script 
        nglessScriptFname = NGOFilename fname
    _ <- htmlResourcePath >>= setupHtmlViewer fname
    r <- evalStateT (runErrorT (interpretIO es)) (0, Map.insert ".scriptfname" nglessScriptFname (Map.insert ".script" nglessScript Map.empty))
    case r of
        Right _ -> getNglessTempDir >>= removeDirectoryRecursive
        Left err -> putStrLn (show err)

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
    let r = evalBinary bop v1' v2'
    return r
interpretExpr (IndexExpression expr ie) = do
    expr' <- interpretExpr expr
    ie' <- interpretIndex ie
    let r = evalIndex expr' ie'
    return r

interpretExpr (ListExpression e) = NGOList <$> mapM interpretExpr e
interpretExpr _ = throwError "Not an expression"


interpretIndex :: Index -> InterpretationROEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: (Maybe Expression) -> InterpretationROEnv (Maybe NGLessObject)
maybeInterpretExpr Nothing = return Nothing
maybeInterpretExpr (Just e) = Just <$> interpretExpr e

variableName :: Expression -> T.Text
variableName (Lookup (Variable n)) = n
variableName _ = error "Should have been used a Lookup variable"

topFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction Ffastq expr _args _block = do
    expr' <- interpretTopValue expr
    executeQualityProcess expr'

topFunction Funique expr args _block = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    executeUnique expr' args'

topFunction Fpreprocess expr args (Just _block) = do
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    res' <- executePreprocess expr' args' _block v >>= executeQualityProcess 
    setVariableValue v res'
    return res'
  where v = variableName expr

topFunction Fwrite expr args _ = do 
    expr' <- interpretTopValue expr
    args' <- runInROEnvIO $ interpretArguments args
    r <- getScriptName
    liftIO (writeToFile expr' args' r)

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


topFunction _ _ _ _ = throwError "Unable to handle these functions"

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
        a = fromMaybe False $ evalBool <$> lookup "keep_ambiguous" args
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
    newTemplate <- liftIO $ generateDirId r fname' -- new template only calculated once.
    _ <- liftIO $ insertFilesProcessedJson newTemplate (T.pack r)
    executeQualityProcess' Nothing fname' "beforeQC" newTemplate

executeQualityProcess _ = throwError("Should be passed a ConstStr or [ConstStr]")
executeQualityProcess' enc fname info nt = liftIO $ executeQProc enc fname info nt

executeMap :: NGLessObject -> [(T.Text, NGLessObject)] -> InterpretationEnvIO NGLessObject
executeMap (NGOList e) args = return . NGOList =<< mapM (\x -> executeMap x args) e
executeMap (NGOReadSet file _enc _) args = case lookup "reference" args of 
                Just refPath' -> liftIO $ interpretMapOp (evalString refPath') file
                Nothing       -> error ("A reference must be suplied")

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
            _ <- liftIO $ removeDirectoryRecursive d
            return $ NGOReadSet nFp enc t

executeUnique _ _ = error "executeUnique: Should not have happened"


executePreprocess :: NGLessObject -> [(T.Text, NGLessObject)] -> Block -> T.Text -> InterpretationEnvIO NGLessObject
executePreprocess (NGOList e) args _block v = return . NGOList =<< mapM (\x -> executePreprocess x args _block v) e
executePreprocess (NGOReadSet file enc t) args (Block ([Variable var]) expr) _ = do
        liftIO $printNglessLn $ "ExecutePreprocess on " ++ file
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
                        Just value -> case evalInteger $ evalLen value of
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
evalUOP UOpMinus x@(NGOInteger _) = evalMinus x
evalUOP UOpLen sr@(NGOShortRead _) = evalLen sr
evalUOP _ _ = error "invalid unary operation. "

evalLen (NGOShortRead r) = NGOInteger . toInteger $ srLength r
evalLen err = error ("Length must receive a Read. Received a " ++ show err)

evalMinus (NGOInteger n) = NGOInteger (-n)
evalMinus err = error ("Minus operator must receive a integer. Received a" ++ show err)

evalIndex :: NGLessObject -> [Maybe NGLessObject] -> NGLessObject
evalIndex sr index@[Just (NGOInteger a)] = evalIndex sr $ (Just $ NGOInteger (a + 1)) : index
evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Nothing] =
    NGOShortRead $ ShortRead rId (B.drop (fromIntegral s) rSeq) (B.drop (fromIntegral s) rQual)
evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Nothing, Just (NGOInteger e)] =
    NGOShortRead $ ShortRead rId (B.take (fromIntegral e) rSeq) (B.take (fromIntegral e) rQual)

evalIndex (NGOShortRead (ShortRead rId rSeq rQual)) [Just (NGOInteger s), Just (NGOInteger e)] = do
    let e' = (fromIntegral e)
        s' = (fromIntegral s)
        e'' = e'- s'
    NGOShortRead (ShortRead rId (B.take e'' . B.drop s' $ rSeq) (B.take e'' . B.drop s' $ rQual))
evalIndex _ _ = error "evalIndex: invalid operation"

evalBool (NGOBool x) = x
evalBool _ = error "evalBool: Argument type must be NGOBool"

evalString (NGOString s) = s
evalString o = error ("evalString: Argument type must be NGOString (received " ++ show o ++ ").")

evalSymbol (NGOSymbol s) = s
evalSymbol o = error ("evalSymbol: Argument type must be NGOSymbol (received " ++ show o ++ ").")

evalInteger (NGOInteger i) = i
evalInteger o = error ("evalInteger: Argument type must be NGOInteger (got " ++ show o ++ ").")


-- Binary Evaluation
evalBinary :: BOp ->  NGLessObject -> NGLessObject -> NGLessObject
evalBinary BOpLT lexpr rexpr = lt lexpr rexpr 
evalBinary BOpGT lexpr rexpr  = gt lexpr rexpr 
evalBinary BOpLTE lexpr rexpr = lte lexpr rexpr
evalBinary BOpGTE lexpr rexpr = gte lexpr rexpr
evalBinary BOpEQ lexpr rexpr =  NGOBool $ lexpr == rexpr
evalBinary BOpNEQ lexpr rexpr =  NGOBool $ lexpr /= rexpr
evalBinary BOpAdd lexpr rexpr =  add lexpr rexpr
evalBinary BOpMul lexpr rexpr =  mul lexpr rexpr 


{- Allows for the addition of new types and operations easily if required. -}

gte (NGOInteger x) (NGOInteger y) = NGOBool $ x >= y
gte _ _ = error "BinaryOP gte: Arguments Should be of type NGOInteger" 
gt (NGOInteger x) (NGOInteger y) = NGOBool $ x > y
gt _ _ = error "BinaryOP lte: Arguments Should be of type NGOInteger"
lte (NGOInteger x) (NGOInteger y) = NGOBool $ x <= y
lte _ _ = error "BinaryOP lte: Arguments Should be of type NGOInteger"
lt (NGOInteger x) (NGOInteger y) = NGOBool $ x < y
lt _ _ = error "BinaryOP lt: Arguments Should be of type NGOInteger"
add (NGOInteger x) (NGOInteger y) = NGOInteger $ x + y
add _ _ = error "BinaryOP add: Arguments Should be of type NGOInteger" 
mul (NGOInteger x) (NGOInteger y) = NGOInteger $ x * y
mul _ _ = error "BinaryOP mul: Arguments Should be of type NGOInteger"


asShortRead (NGOShortRead r) = r
asShortRead _ = error "Short read expected"

getScriptName = do
    -- This cannot fail as we inserted the variable ourselves
    Just (NGOFilename fname) <- runInROEnvIO $ lookupVariable ".scriptfname"
    return fname
