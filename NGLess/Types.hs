{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types
    ( checktypes
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Data.String

import Language


type TypeMap = Map.Map T.Text NGLType
type TypeMSt b = ExceptT T.Text (State (Int,TypeMap)) b

-- | checktypes will either return an error message or pass through the script
checktypes :: Script -> Either T.Text Script
checktypes script@(Script _ exprs) = evalState (runExceptT (inferScriptM exprs >> return script)) (0,Map.empty)

errorInLineC :: [String] -> TypeMSt a
errorInLineC = errorInLine . T.concat . map fromString

errorInLine :: T.Text -> TypeMSt a
errorInLine e = do
    line <- fst `fmap` get
    throwError (T.concat ["Line ", T.pack (show line), ": ", e])

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

inferBlock :: FuncName -> Maybe Block -> TypeMSt ()
inferBlock _ Nothing = return ()
inferBlock f (Just (Block vars es)) = do
        forM_ vars $ \(Variable v) ->
            envInsert v blockArg
        inferM es
    where
        blockArg = case f of
            Fpreprocess -> NGLRead
            Fselect -> NGLMappedRead
            _ -> error ("This function '" ++ show f ++ "' does not accept blocks")

envLookup :: T.Text -> TypeMSt (Maybe NGLType)
envLookup v = Map.lookup v . snd <$> get

envInsert :: T.Text -> NGLType -> TypeMSt ()
envInsert v t = modify (\(lno,m) -> (lno, Map.insert v t m))

check_assignment :: Maybe NGLType -> Maybe NGLType -> TypeMSt ()
check_assignment _ (Just NGLVoid) = errorInLine "Assigning void value to variable"
check_assignment Nothing _ = return ()
check_assignment a b = when (a /= b)
        (errorInLine $ T.concat ["Assigning type ", showType b, " to a variable that has type ", showType a])
    where
        showType = T.pack . show . fromJust

nglTypeOf :: Expression -> TypeMSt (Maybe NGLType)
nglTypeOf (FunctionCall f arg args b) = inferBlock f b *> checkfuncargs f args *> checkfunccall f arg
nglTypeOf (MethodCall m self arg args) = checkmethodargs m args *> checkmethodcall m self arg
nglTypeOf (Lookup (Variable v)) = envLookup v
nglTypeOf (BuiltinConstant (Variable v)) = return (typeOfConstant v)
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
nglTypeOf Condition{}    = error "unexpected nglTypeOf(Condition)"
nglTypeOf (Sequence _es) = error "unexpected nglTypeOf(Sequence)"

checkuop UOpLen e = checklist e *> return (Just NGLInteger)
checkuop UOpMinus e = checkinteger e
checkuop UOpNot e = checkbool e

checkbop BOpAdd a b = checkinteger a *> checkinteger b
checkbop BOpMul a b = checkinteger a *> checkinteger b 

checkbop BOpGT  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpGTE a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpLT  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpLTE a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpEQ  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpNEQ a b = checkinteger a *> checkinteger b *> return (Just NGLBool)


checkbool (ConstBool _) = return (Just NGLBool)
checkbool expr = do
    t <- nglTypeOf expr
    if t /= Just NGLBool
        then  errorInLineC ["Expected boolean expression, got ", show t, " for expression ", show expr]
        else return t

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
    unless (all (==t) ts)
        (errorInLine "List of mixed type")
    return (Just t)
checklist (Lookup (Variable v)) = do
    vtype <- envLookup v
    case vtype of
        Just (NGList btype) -> return (Just btype)
        Just NGLRead -> return (Just NGLRead)
        e -> errorInLine $ T.concat ["List expected. Type ", T.pack . show $ e , " provided."]
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
        checkfunctype NGLAny NGLVoid = errorInLineC
                                    ["Function '", show f, "' can take any type, but the input is of illegal type Void."]
        checkfunctype NGLAny _ = return ()
        checkfunctype t t'
                | t /= t' = errorInLineC
                                    ["Bad type in function call (function '", show f,"' expects ", show t, " got ", show t', ")."]
                | otherwise = return ()

checkfuncargs :: FuncName -> [(Variable, Expression)] -> TypeMSt ()
checkfuncargs f args = mapM_ (checkfuncarg f) args

checkfuncarg :: FuncName -> (Variable, Expression) -> TypeMSt ()
checkfuncarg f (v, e) = do
    eType <- nglTypeOf e
    arg_type <- checkfuncarg' f v
    case eType of
        Just x  -> checkargtype x arg_type *> return ()
        Nothing -> errorInLine "Could not infer type of argument"
    where
        checkargtype t t' = when (t /= t') (errorInLineC
                            ["Bad argument type in ", show f ,", variable " , show v,". expects ", show t', " got ", show t, "."])

requireType :: Expression -> TypeMSt NGLType
requireType e = nglTypeOf e >>= \case
    Nothing -> errorInLineC ["Could not infer required type of expression (", show e, ")"]
    Just t -> return t

checkmethodcall :: MethodName -> Expression -> (Maybe Expression) -> TypeMSt (Maybe NGLType)
checkmethodcall m self arg = do
    stype <- requireType self
    let reqSelfType = methodSelfType m
    when (stype /= reqSelfType) (errorInLineC
        ["Wrong type for method ", show m, ". This method is defined for type ", show reqSelfType,
         ", but expression (", show self, ") has type ", show stype])
    argType <- maybe (return Nothing) nglTypeOf arg
    let reqArgType = methodArgType m
    case (argType,reqArgType) of
        (Nothing, _) -> return ()
        (Just _, Nothing) -> errorInLineC ["Method ", show m, " does not take any unnamed argument"]
        (Just t, Just t') -> when (t /= t') (errorInLineC
                        ["Method ", show m, " expects type ", show t', " got ", show t])
    return . Just . methodReturnType $ m

checkmethodargs :: MethodName -> [(Variable, Expression)] -> TypeMSt ()
checkmethodargs m args = forM_ args check1arg
    where
        check1arg (v, e) = do
            actualType <- requireType e
            let reqType = methodKwargType m v
            when (actualType /= reqType) (errorInLineC
                    ["Bad argument type for argument ", show v, " in method call ", show m, ". ",
                     "Expected ", show reqType, " got ", show actualType])


checkfuncarg' f v = case function_opt_arg_type f v of
    Left  err -> errorInLine err
    Right e   -> return e


