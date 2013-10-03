{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpret
    ( interpret
    , interpretBlock
    ) where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe
import Data.String

import ProcessFastQ
import FPreProcess
import Language

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
type InterpretationEnvIO = InterpretationEnvT IO
type InterpretationEnv = InterpretationEnvT Identity
type InterpretationROEnv =
            ErrorT
                NGError
                (Reader NGLEnv_t)

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

runInROEnvIO :: InterpretationROEnv a -> InterpretationEnvIO a
runInROEnvIO = runInEnv . runInROEnv

interpret :: [(Int,Expression)] -> IO ()
interpret es = do
    r <- evalStateT (runErrorT (interpretIO es)) (0, Map.empty)
    case r of
        Right _ -> return ()
        Left err -> putStrLn (show err)

interpretIO :: [(Int, Expression)] -> InterpretationEnvIO ()
interpretIO [] = return ()
interpretIO ((ln,e):es) = (setlno ln >> interpretTop e >> interpretIO es)

interpretTop :: Expression -> InterpretationEnvIO ()
interpretTop (Assignment (Variable var) val) = do
    val' <- interpretTopValue val
    setVariableValue var val'
    return ()
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
interpretExpr (UnaryOp UOpMinus v) = interpretExpr v >>= return . evalMinus
interpretExpr (UnaryOp UOpLen ell) = interpretExpr ell >>= return . evalLen
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
interpretExpr _ = throwError "Not an expression"

interpretIndex :: Index -> InterpretationROEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: (Maybe Expression) -> InterpretationROEnv (Maybe NGLessObject)
maybeInterpretExpr Nothing = return Nothing
maybeInterpretExpr (Just e) = interpretExpr e >>= return . Just

topFunction :: FuncName -> Expression -> [(Variable, Expression)] -> Maybe Block -> InterpretationEnvIO NGLessObject
topFunction Ffastq (ConstStr fname) _args _block = liftIO (readFastQ (T.unpack fname)) >> return NGOVoid
topFunction Fpreprocess expr args (Just block) = do
    expr' <- runInROEnvIO $ interpretExpr expr
    args' <- runInROEnvIO $ evaluateArguments args
    executePreprocess expr' args' block
    return NGOVoid
topFunction _ _ _ _ = throwError ("Unable to handle these functions")
executePreprocess _ _ _ = return ()

evaluateArguments [] = return []
evaluateArguments ((v,e):args) = do
    e' <- interpretExpr e
    args' <- evaluateArguments args
    return ((v,e'):args')

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
    if evalBool v'
        then interpretBlock1 vs ifT
        else interpretBlock1 vs ifF
interpretBlock1 _ _ = error "should not have gotten here"

interpretBlockExpr :: [(T.Text, NGLessObject)] -> Expression -> InterpretationROEnv NGLessObject
interpretBlockExpr vs val = local (\e -> Map.union e (Map.fromList vs)) (interpretExpr val)

evalMinus (NGOInteger n) = NGOInteger (-n)
evalMinus _ = error "invalid minus operation"
evalLen _ = error "not implemented yet"
evalBinary _bop _ _ = error "not implemented yet"
evalIndex _ _ = error "not implemented yet"
evalBool _ = error "not implemented yet"
