{- Copyright 2013-2016 NGLess Authors
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
import BuiltinFunctions
import Utils.Suggestion


type TypeMap = Map.Map T.Text NGLType
type TypeMSt = StateT (Int, TypeMap)        -- ^ Current line & current type map (type map is inferred top-to-bottom)
                (MaybeT                     -- ^ to enable early exit for certain types of error
                    (ReaderT [Module]       -- ^ the modules passed in (fixed)
                        (Writer [T.Text]))) -- ^ we write out error messages

-- | checktypes will either return an error message or pass through the script
checktypes :: [Module] -> Script -> Either T.Text Script
checktypes mods script@(Script _ exprs) = let w = runMaybeT (runStateT (inferScriptM exprs) (0,Map.empty)) in
    case runWriter (runReaderT w mods) of
        (Just _, []) -> Right script
        (_, errs) -> Left (T.unlines errs)

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
inferM (Condition c te fe) = checkBool c *> inferM te *> inferM fe
inferM e = void (nglTypeOf e)

inferBlock :: FuncName -> Maybe Block -> TypeMSt ()
inferBlock _ Nothing = return ()
inferBlock (FuncName f) (Just (Block vars es)) = case f of
        "preprocess" -> inferBlock' NGLRead
        "select" -> inferBlock' NGLMappedRead
        _ -> do
            errorInLineC ["Function '", T.unpack f, "' does not accept blocks"]
            void $ cannotContinue
    where
        inferBlock' btype = do
            forM_ vars $ \(Variable v) ->
                envInsert v btype
            inferM es

envLookup :: T.Text -> TypeMSt (Maybe NGLType)
envLookup v = (liftM2 (<|>)) (constantLookup v) (Map.lookup v . snd <$> get)

constantLookup :: T.Text -> TypeMSt (Maybe NGLType)
constantLookup v = do
    moduleBuiltins <- concat . map modConstants <$> ask
    case filter ((==v) . fst) moduleBuiltins of
        [] -> return Nothing
        [(_,r)] -> return $ typeOfObject r
        _ -> do
            errorInLineC ["Multiple matches for constant: ", T.unpack v]
            cannotContinue

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
nglTypeOf (FunctionCall f arg args b) = inferBlock f b *> checkFuncKwArgs f args *> checkFuncUnnamed f arg
nglTypeOf (MethodCall m self arg args) = checkmethodargs m args *> checkmethodcall m self arg
nglTypeOf (Lookup (Variable v)) = envLookup v
nglTypeOf (BuiltinConstant (Variable v)) = return (typeOfConstant v)
nglTypeOf (ConstStr _) = return (Just NGLString)
nglTypeOf (ConstInt _) = return (Just NGLInteger)
nglTypeOf (ConstDouble _) = return (Just NGLDouble)
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
nglTypeOf Optimized{}    = error "unexpected nglTypeOf(Optimized)"
nglTypeOf Condition{}    = error "unexpected nglTypeOf(Condition)"
nglTypeOf (Sequence _es) = error "unexpected nglTypeOf(Sequence)"


typeOfObject :: NGLessObject -> Maybe NGLType
typeOfObject (NGOString _) = Just NGLString
typeOfObject (NGOBool _) = Just NGLBool
typeOfObject (NGOInteger _) = Just NGLInteger
typeOfObject (NGODouble _) = Just NGLDouble
typeOfObject (NGOSymbol _) = Just NGLSymbol
typeOfObject (NGOFilename _) = Just NGLFilename
typeOfObject (NGOShortRead _) = Just NGLRead
typeOfObject NGOReadSet{} = Just NGLReadSet
typeOfObject NGOMappedReadSet{} = Just NGLMappedReadSet
typeOfObject NGOMappedRead{} = Just NGLMappedRead
typeOfObject NGOCounts{} = Just NGLCounts
typeOfObject NGOVoid = Just NGLVoid
typeOfObject (NGOList []) = Nothing
typeOfObject (NGOList (v:_)) = NGList <$> typeOfObject v
typeOfObject (NGOExpression _) = error "unexpected typeOfObject(NGOExpression)"


checkuop UOpLen e = checklist e *> return (Just NGLInteger)
checkuop UOpMinus e = checkinteger e
checkuop UOpNot e = checkBool e

checkbop :: BOp -> Expression -> Expression -> TypeMSt (Maybe NGLType)
checkbop BOpAdd a b = do
    t <- liftM2 (<|>)
        (softCheckInteger a *> softCheckInteger b)
        (softCheckString  a *> softCheckString b)
    when (t == Nothing) $
        errorInLineC ["Addition operator (+) must be applied to a pair of strings or integers"]
    return t
checkbop BOpMul a b = checkinteger a *> checkinteger b

checkbop BOpGT  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpGTE a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpLT  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpLTE a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpEQ  a b = checkinteger a *> checkinteger b *> return (Just NGLBool)
checkbop BOpNEQ a b = checkinteger a *> checkinteger b *> return (Just NGLBool)


softCheck :: NGLType -> Expression -> TypeMSt (Maybe NGLType)
softCheck expected expr = do
    t <- nglTypeOf expr
    return $ if t /= Just expected
        then Nothing
        else t

softCheckInteger = softCheck NGLInteger
softCheckString  = softCheck NGLString

checkBool (ConstBool _) = return (Just NGLBool)
checkBool expr = do
    t <- nglTypeOf expr
    when (t /= Just NGLBool) $
        errorInLineC ["Expected boolean expression, got ", show t, " for expression ", show expr]
    return (Just NGLBool)

checkinteger (ConstInt _) = return (Just NGLInteger)
checkinteger expr = do
    t <- nglTypeOf expr
    when (t /= Just NGLInteger) $
        errorInLineC ["Expected integer expression, got ", show t, " for expression ", show expr]
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
            errorInLineC (["Unknown function '", show fn, "'"] ++ case findSuggestion (unwrapFuncName fn) (unwrapFuncName . funcName <$> fs) of
                                        Nothing -> []
                                        Just (Suggestion sug reason) -> [". Did you mean `", T.unpack sug, "` (", T.unpack reason, ")?"])
            cannotContinue
        _ -> do
            errorInLineC ["Too many matches for function '", show fn, "'"]
            cannotContinue

checkFuncUnnamed :: FuncName -> Expression -> TypeMSt (Maybe NGLType)
checkFuncUnnamed f arg = do
        targ <- nglTypeOf arg
        Function _ (Just etype) rtype _ allowAutoComp <- funcInfo f
        case targ of
            Just (NGList t)
                | allowAutoComp -> checkfunctype etype t *> return (Just (NGList rtype))
            Just t -> checkfunctype etype t *> return (Just rtype)
            Nothing -> do
                errorInLineC ["While checking types for function ", show f, "Could not infer type of argument (saw :", show arg, ")"]
                cannotContinue
    where
        checkfunctype NGLAny NGLVoid = errorInLineC
                                    ["Function '", show f, "' can take any type, but the input is of illegal type Void."]
        checkfunctype NGLAny _ = return ()
        checkfunctype t t'
                | t /= t' = errorInLineC
                                    ["Bad type in function call (function '", show f,"' expects ", show t, " got ", show t', ")."]
                | otherwise = return ()

checkFuncKwArgs :: FuncName -> [(Variable, Expression)] -> TypeMSt ()
checkFuncKwArgs f args = do
    Function _ _ _ argInfo _ <- funcInfo f
    mapM_ (checkfuncarg f argInfo) args


checkfuncarg :: FuncName -> [ArgInformation] -> (Variable, Expression) -> TypeMSt ()
checkfuncarg f arginfo (Variable v, e) = do
    eType <- nglTypeOf e
    let ainfo = find ((==v) . argName) arginfo
    case (ainfo,eType) of
        (Nothing, _) -> errorInLineC $ ["Bad argument '", T.unpack v, "' for function '", show f, "'.\n"]
                            ++ (case findSuggestion v (argName <$> arginfo) of
                                Just (Suggestion valid reason) -> ["\tDid you mean `", T.unpack valid, "` (", T.unpack reason, ").\n\n"]
                                Nothing -> []
                            ) ++ ["This function takes the following arguments:\n"]
                            ++ (map ((\aname -> ("\t"++aname++"\n")) . T.unpack . argName) arginfo)
        (_, Nothing) -> errorInLine "Could not infer type of argument"
        (Just ainfo', Just t') -> when (argType ainfo' /= t') $
                    (errorInLineC
                            ["Bad argument type in ", show f ,", variable " , show v, ". ",
                            "Expected ", show . argType $ ainfo', " got ", show t', "."])

requireType :: NGLType -> Expression -> TypeMSt NGLType
requireType def_t e = nglTypeOf e >>= \case
    Nothing -> do
        errorInLineC ["Could not infer required type of expression (", show e, ")"]
        return def_t
    Just t -> return t

checkmethodcall :: MethodName -> Expression -> (Maybe Expression) -> TypeMSt (Maybe NGLType)
checkmethodcall m self arg = do
    let minfo = findMethodInfo m
        reqSelfType = methodSelfType minfo
        reqArgType = methodArgType minfo
    stype <- requireType reqSelfType self
    when (stype /= reqSelfType) (errorInLineC
        ["Wrong type for method ", show m, ". This method is defined for type ", show reqSelfType,
         ", but expression (", show self, ") has type ", show stype])
    actualType <- maybe (return Nothing) nglTypeOf arg
    case (actualType, reqArgType) of
        (Nothing, _) -> return ()
        (Just _, Nothing) -> errorInLineC ["Method ", show m, " does not take any unnamed argument (saw ", show arg, ")"]
        (Just t, Just t') -> when (t /= t') (errorInLineC
                        ["Method ", show m, " expects type ", show t', " got ", show t])
    return . Just . methodReturnType $ minfo

checkmethodargs :: MethodName -> [(Variable, Expression)] -> TypeMSt ()
checkmethodargs m args = forM_ args check1arg *> findAllRequired
    where
        minfo = findMethodInfo m
        requiredArgs = filter argRequired (methodKwargsInfo minfo)
        findAllRequired = mapM_ find1 requiredArgs

        find1 :: ArgInformation -> TypeMSt ()
        find1 ai = case filter (\(Variable v,_) -> v == argName ai) args of
            [_] -> return ()
            [] -> errorInLineC ["Argument ", T.unpack (argName ai), " is missing in method call ", show m, "."]
            _ -> error "This should never happen: multiple arguments with the same name should have been caught before"
        check1arg (Variable v, e) = do
            case filter ((==v) . argName) (methodKwargsInfo minfo) of
                [] -> errorInLineC ["Argument ", show v, " in method call ", show m, ": not recognized. ", T.unpack $ suggestionMessage v (argName <$> methodKwargsInfo minfo)]
                [ainfo] -> do
                    let reqType = argType ainfo
                    actualType <- requireType reqType e
                    when (actualType /= reqType) (errorInLineC
                            ["Bad argument type for argument ", show v, " in method call ", show m, ". ",
                             "Expected ", show reqType, " got ", show actualType])
                _ -> error "This should never happen. Internal bug in ngless."




findMethodInfo m = head (filter ((==m) . methodName) builtinMethods)
