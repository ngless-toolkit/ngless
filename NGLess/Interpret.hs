{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpret
    ( interpret
    ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as Map
import Data.Text as T
import Data.Maybe
import Data.String

import ProcessFastQ
import FPreProcess
import Language

data NGError = NGError !T.Text
        deriving (Show, Eq)

instance Error NGError where
    strMsg = NGError . T.pack

instance IsString NGError where
    fromString = NGError . T.pack

type NGLEnv_t = Map.Map T.Text NGLessObject

type InterpretationEnvT m a =
            ErrorT
                NGError
                (StateT (Int,NGLEnv_t) m)
                a
type InterpretationEnvIO a = InterpretationEnvT IO a
type InterpretationEnv a = InterpretationEnvT Identity a

setlno :: Int -> InterpretationEnvIO ()
setlno !n = modify $ \(_,e) -> (n,e)

lookupVariable :: T.Text -> InterpretationEnv (Maybe NGLessObject)
lookupVariable !k = gets $ \(_,e) -> Map.lookup k e

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
interpretTop func@(FunctionCall _ _ _ _ ) = void $ topFunction func
interpretTop (Condition c ifTrue ifFalse) = do
    c' <- runInEnv (interpretExpr c)
    if evalBool c'
        then interpretTop ifTrue
        else interpretTop ifFalse
interpretTop (Sequence es) = forM_ es interpretTop
interpretTop _ = throwError "Top level statement is NOP"

interpretTopValue :: Expression -> InterpretationEnvIO NGLessObject
interpretTopValue f@(FunctionCall {}) = topFunction f
interpretTopValue e = runInEnv (interpretExpr e)

interpretExpr :: Expression -> InterpretationEnv NGLessObject
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

interpretIndex :: Index -> InterpretationEnv [Maybe NGLessObject]
interpretIndex (IndexTwo a b) = forM [a,b] maybeInterpretExpr
interpretIndex (IndexOne a) = forM [Just a] maybeInterpretExpr

maybeInterpretExpr :: (Maybe Expression) -> InterpretationEnv (Maybe NGLessObject)
maybeInterpretExpr Nothing = return Nothing
maybeInterpretExpr (Just e) = interpretExpr e >>= return . Just

topFunction :: Expression -> InterpretationEnvIO NGLessObject
topFunction (FunctionCall Ffastq (ConstStr fname) _exprs _block) = liftIO (readFastQ (T.unpack fname)) >> return NGOVoid
topFunction (FunctionCall Fpreprocess expr args (Just block)) = do
    expr' <- runInEnv $ interpretExpr expr
    args' <- runInEnv $ evaluateArguments args
    executePreprocess expr' args' block
    return NGOVoid
topFunction _ = throwError ("Unable to handle these functions")

executePreprocess _ _ _ = return ()

evaluateArguments [] = return []
evaluateArguments ((v,e):args) = do
    e' <- interpretExpr e
    args' <- evaluateArguments args
    return ((v,e'):args')

evalMinus (NGOInteger n) = NGOInteger (-n)
evalMinus _ = error "invalid minus operation"
evalLen _ = error "not implemented yet"
evalBinary _bop _ _ = error "not implemented yet"
evalIndex _ _ = error "not implemented yet"
evalBool _ = False
