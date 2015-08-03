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
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.String
import Data.List (find)

import Modules
import Language
import Functions


type TypeMap = Map.Map T.Text NGLType
type TypeMSt b = StateT (Int, TypeMap) (MaybeT (ReaderT [Module] (Writer [T.Text]))) b

-- | checktypes will either return an error message or pass through the script
checktypes :: [Module] -> Script -> Either T.Text Script
checktypes mods script@(Script _ exprs) = let w = runMaybeT (runStateT (inferScriptM exprs) (0,Map.empty)) in
    case runWriter (runReaderT w mods) of
        (Nothing, errs) -> Left (T.concat errs)
        (Just ((),_), []) -> Right script
        (Just _,  errs) -> Left (T.concat errs)

errorInLineC :: [String] -> TypeMSt ()
errorInLineC = errorInLine . T.concat . map fromString

errorInLine :: T.Text -> TypeMSt ()
errorInLine e = do
    (line,_) <- get
    tell [T.concat ["Error in type-checking (line ", T.pack (show line), "): ", e]]

-- | There are two types of errors: errors which can potentially be recovered
-- from (mostly by pretending that the type was what was expected and
-- continuing): these get accumulated until the end of parsing. Some errors,
-- however, trigger an immediate abort by calling 'cannotContinue'
cannotContinue :: TypeMSt a
cannotContinue = lift (MaybeT (return Nothing))


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
inferBlock (FuncName f) (Just (Block vars es)) = do
        forM_ vars $ \(Variable v) ->
            envInsert v blockArg
        inferM es
    where
        blockArg = case f of
            "preprocess" -> NGLRead
            "select" -> NGLMappedRead
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
        then do
            errorInLineC ["Expected boolean expression, got ", show t, " for expression ", show expr]
            return (Just NGLBool)
        else return t

checkinteger (ConstNum _) = return (Just NGLInteger)
checkinteger expr = do
    t <- nglTypeOf expr
    when (t /= Just NGLInteger) $
        errorInLine "Expected integer"
    return (Just NGLInteger)

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
        e -> do
            errorInLine $ T.concat ["List expected. Type ", T.pack . show $ e , " provided."]
            return (Just NGLVoid)
checklist _ = errorInLine "List expected" >> return (Just NGLVoid)

allFunctions = (builtinFunctions ++) <$> moduleFunctions
moduleFunctions = concat . map modFunctions <$> ask

funcInfo fn = do
    fs <- allFunctions
    let matched = filter ((==fn) . funcName) fs
    case matched of
        [fi] -> return fi
        [] -> do
            errorInLineC ["Unknown function '", show fn, "'"]
            cannotContinue
        _ -> do
            errorInLineC ["Too many matches for function '", show fn, "'"]
            cannotContinue

checkfunccall :: FuncName -> Expression -> TypeMSt (Maybe NGLType)
checkfunccall f arg = do
        targ <- nglTypeOf arg
        Function _ (Just etype) rtype _ _ <- funcInfo f
        case targ of
            Just (NGList t) -> checkfunctype etype t *> return (Just (NGList rtype))
            Just t -> checkfunctype etype t *> return (Just rtype)
            Nothing -> do
                errorInLine "Could not infer type of argument"
                cannotContinue
    where
        checkfunctype NGLAny NGLVoid = errorInLineC
                                    ["Function '", show f, "' can take any type, but the input is of illegal type Void."]
        checkfunctype NGLAny _ = return ()
        checkfunctype t t'
                | t /= t' = errorInLineC
                                    ["Bad type in function call (function '", show f,"' expects ", show t, " got ", show t', ")."]
                | otherwise = return ()

checkfuncargs :: FuncName -> [(Variable, Expression)] -> TypeMSt ()
checkfuncargs f args = do
    Function _ _ _ argInfo _ <- funcInfo f
    mapM_ (checkfuncarg f argInfo) args


checkfuncarg :: FuncName -> [ArgInformation] -> (Variable, Expression) -> TypeMSt ()
checkfuncarg f arginfo (Variable v, e) = do
    eType <- nglTypeOf e
    let ainfo = find ((==v) . argName) arginfo
    case (ainfo,eType) of
        (Nothing, _) -> errorInLineC ["Bad argument '", T.unpack v, "' for function '", show f, "'"]
        (_, Nothing) -> errorInLine "Could not infer type of argument"
        (Just ainfo', Just t') -> when (argType ainfo' /= t') $
                    (errorInLineC
                            ["Bad argument type in ", show f ,", variable " , show v,". expects ", show t', " got ", show . argType $ ainfo', "."])

requireType :: NGLType -> Expression -> TypeMSt NGLType
requireType def_t e = nglTypeOf e >>= \case
    Nothing -> do
        errorInLineC ["Could not infer required type of expression (", show e, ")"]
        return def_t
    Just t -> return t

checkmethodcall :: MethodName -> Expression -> (Maybe Expression) -> TypeMSt (Maybe NGLType)
checkmethodcall m self arg = do
    let reqSelfType = methodSelfType m
    stype <- requireType reqSelfType self
    when (stype /= reqSelfType) (errorInLineC
        ["Wrong type for method ", show m, ". This method is defined for type ", show reqSelfType,
         ", but expression (", show self, ") has type ", show stype])
    actualType <- maybe (return Nothing) nglTypeOf arg
    let reqArgType = methodArgType m
    case (actualType, reqArgType) of
        (Nothing, _) -> return ()
        (Just _, Nothing) -> errorInLineC ["Method ", show m, " does not take any unnamed argument"]
        (Just t, Just t') -> when (t /= t') (errorInLineC
                        ["Method ", show m, " expects type ", show t', " got ", show t])
    return . Just . methodReturnType $ m

checkmethodargs :: MethodName -> [(Variable, Expression)] -> TypeMSt ()
checkmethodargs m args = forM_ args check1arg
    where
        check1arg (v, e) = do
            let reqType = methodKwargType m v
            actualType <- requireType reqType e
            when (actualType /= reqType) (errorInLineC
                    ["Bad argument type for argument ", show v, " in method call ", show m, ". ",
                     "Expected ", show reqType, " got ", show actualType])



