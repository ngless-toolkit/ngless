{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types
    ( checktypes
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.String

import Language


type TypeMap = Map.Map T.Text NGLType
type TypeMSt b = ErrorT T.Text (State (Int,TypeMap)) b
instance Error T.Text where
    strMsg = T.pack

-- | checktypes will either return an error message or pass through the script
checktypes :: Script -> Either T.Text Script
checktypes script@(Script _ exprs) = evalState (runErrorT (inferScriptM exprs >> return script)) (0,Map.empty)


errorInLineC :: [String] -> TypeMSt ()
errorInLineC = errorInLine . T.concat . map fromString

errorInLine :: T.Text -> TypeMSt a
errorInLine e = do
    line <- fst `fmap` get
    throwError (T.concat ["Line ", T.pack (show line),": ", e])

inferScriptM :: [(Int,Expression)] -> TypeMSt ()
inferScriptM [] = return ()
inferScriptM ((lno,e):es) = modify (\(_,m) -> (lno,m)) >> inferM e >> inferScriptM es

inferM :: Expression -> TypeMSt ()
inferM (Sequence es) = inferM `mapM_` es
inferM (Assignment (Variable v) expr) = do
    ltype <- envLookup v
    mrtype <- nglTypeOf expr
    check_assignment ltype mrtype
    case mrtype of
        Nothing -> errorInLine "Cannot infer type for right-hand of assignment"
        Just rtype -> envInsert v rtype
inferM (Condition c te fe) = checkbool c >> inferM te >> inferM fe
inferM e = void (nglTypeOf e)

envLookup :: T.Text -> TypeMSt (Maybe NGLType)
envLookup v = Map.lookup v . snd <$> get

envInsert :: T.Text -> NGLType -> TypeMSt ()
envInsert v t = modify (\(lno,m) -> (lno, Map.insert v t m))

check_assignment :: Maybe NGLType -> Maybe NGLType -> TypeMSt ()
check_assignment _ (Just NGLVoid) = errorInLine "Assigning void value to variable"
check_assignment Nothing _ = return ()
check_assignment a b = guard (a == b)

nglTypeOf :: Expression -> TypeMSt (Maybe NGLType)
nglTypeOf (FunctionCall f arg _ _) = checkfunccall f arg
nglTypeOf (Lookup (Variable v)) = envLookup v
nglTypeOf (ConstStr _) = return (Just NGLString)
nglTypeOf (ConstNum _) = return (Just NGLInteger)
nglTypeOf (ConstBool _) = return (Just NGLBool)
nglTypeOf (ConstSymbol _) = return (Just NGLSymbol)
nglTypeOf e@(ListExpression _) = do
    mt <- checklist e
    case mt of
        Nothing -> return Nothing
        Just t -> return (Just (NGList t))
nglTypeOf Continue = return Nothing
nglTypeOf Discard = return Nothing
nglTypeOf (Assignment _ expr) = nglTypeOf expr
nglTypeOf (UnaryOp uop expr) = checkuop uop expr
nglTypeOf (BinaryOp bop a b) = checkbop bop a b
nglTypeOf (IndexExpression expr index) = checkindex expr index
nglTypeOf (Condition _ _ _) = error "unexpected nglTypeOf(Condition)"
nglTypeOf (Sequence _es) = error "unexpected nglTypeOf(Sequence)"

checkuop UOpLen e = checklist e *> return (Just NGLInteger)
checkuop UOpMinus e = checkinteger e

checkbop _ a b = checkinteger a *> checkinteger b

checkbool (ConstBool _) = return (Just NGLBool)
checkbool _ = errorInLine "Expected boolean"

checkinteger (ConstNum _) = return (Just NGLInteger)
checkinteger expr = do
    t <- nglTypeOf expr
    if t /= Just NGLInteger
        then errorInLine "Expected integer"
        else return t

checkindex expr index = checkindex' index *> checklist expr
    where
        checkindex' (IndexOne e) = checkinteger e
        checkindex' (IndexTwo a b) = checkinteger' a *> checkinteger' b
        checkinteger' Nothing = return Nothing
        checkinteger' (Just v) = checkinteger v

checklist (ListExpression []) = return (Just NGLVoid)
checklist (ListExpression es) = do
    types <- nglTypeOf `mapM` es
    let (t:ts) = catMaybes types
    when (not $ all (==t) ts)
        (errorInLine "List of mixed type")
    return (Just t)
checklist (Lookup (Variable v)) = do
    vtype <- envLookup v
    case vtype of
        Just (NGList btype) -> return (Just btype)
        _ -> errorInLine "List expected"
checklist _ = errorInLine "List expected"

checkfunccall :: FuncName -> Expression -> TypeMSt (Maybe NGLType)
checkfunccall f arg = do
        targ <- nglTypeOf arg
        let etype = function_arg_type f
            rtype = function_return_type f
        case targ of
            Just (NGList t) -> checkfunctype etype t *> return (Just (NGList rtype))
            Just t -> checkfunctype etype t *> return (Just rtype)
            Nothing -> errorInLine "Could not infer type of argument"
    where
        checkfunctype t t' = when (t /= t') (errorInLineC
                                    ["Bad type in function call (function '", show f,"' expects ", show t, " got ", show t', ")."])
