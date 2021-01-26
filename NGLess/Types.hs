{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
module Types
    ( checktypes
    ) where

{-| # Type Checking
 -
 - This module performs type inferrence and checking.
 -}

import qualified Data.Text as T
import qualified Data.Map as Map
import           Control.Arrow
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import           Data.String (fromString)
import           Data.List (find, foldl')
import           Data.Functor (($>))

import Modules
import Language
import NGLess.NGError
import BuiltinFunctions
import Utils.Suggestion
import Utils.Utils


type TypeMap = Map.Map T.Text NGLType
type TypeMSt = StateT (Int, TypeMap)        -- ^ Current line & current type map (type map is inferred top-to-bottom)
                (ExceptT NGError            -- ^ to enable early exit for certain types of error
                    (ReaderT [Module]       -- ^ the modules passed in (fixed)
                        (Writer [T.Text]))) -- ^ we write out error messages

--
-- | checktypes attempts to add types to all the Lookup Expression objects
checktypes :: [Module] -> Script -> NGLess Script
checktypes mods script@(Script _ exprs) = let
            initial = foldl' addmod Map.empty mods
            addmod :: TypeMap -> Module -> TypeMap
            addmod tm m = foldl' addconst tm (modConstants m)
            addconst tm (name, val) = case typeOfObject val of
                Just t -> Map.insert name t tm
                Nothing -> tm
            w = runExceptT (runStateT (inferScriptM exprs) (0,initial))
    in case runWriter (runReaderT w mods) of
        (Right (_,(_, tmap)), []) -> do
            typed <- addTypes tmap exprs
            return $ script { nglBody = typed }
        (Left err, []) -> Left err
        (_, errs) -> throwScriptError . T.unpack . T.unlines $ errs

-- | error in line concat
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
cannotContinue = tell ["Cannot continue typechecking."] >> throwScriptError "Error in type checking"


inferScriptM :: [(Int,Expression)] -> TypeMSt ()
inferScriptM [] = return ()
inferScriptM ((lno,e):es) = modify (first (const lno)) >> inferM e >> inferScriptM es

inferM :: Expression -> TypeMSt ()
inferM (Sequence es) = inferM `mapM_` es
inferM (Assignment (Variable v) expr) = do
    ltype <- envLookup Nothing v
    mrtype <- nglTypeOf expr
    check_assignment ltype mrtype
    case mrtype of
        Nothing -> errorInLine "Cannot infer type for right-hand of assignment"
        Just rtype -> envInsert v rtype
inferM (Condition c te fe) = checkBool c *> inferM te *> inferM fe
inferM e = void (nglTypeOf e)

inferBlock :: FuncName -> Maybe Block -> TypeMSt ()
inferBlock _ Nothing = return ()
inferBlock (FuncName f) (Just (Block (Variable v) es)) = case f of
        "preprocess" -> inferBlock' NGLRead
        "select" -> inferBlock' NGLMappedRead
        _ -> do
            errorInLineC ["Function '", T.unpack f, "' does not accept blocks"]
            void cannotContinue
    where
        inferBlock' btype = do
            envInsert v btype
            inferM es

envLookup :: Maybe NGLType -> T.Text -> TypeMSt (Maybe NGLType)
envLookup Nothing v = envLookup' v
envLookup mt@(Just t) v = envLookup' v >>= \case
        Nothing -> return mt
        Just t'
            | t == t' -> return mt
            | otherwise -> do
                errorInLineC ["Incompatible types detected for variable '"
                                , T.unpack v, "': previously assigned to type "
                                , show t, ", now being detected as ", show t']
                return mt
envLookup' v = liftM2 (<|>)
                    (constantLookup v)
                    (Map.lookup v . snd <$> get)

constantLookup :: T.Text -> TypeMSt (Maybe NGLType)
constantLookup v = do
    moduleBuiltins <- asks (concatMap modConstants)
    case filter ((==v) . fst) moduleBuiltins of
        [] -> return Nothing
        [(_,r)] -> return $ typeOfObject r
        _ -> do
            errorInLineC ["Multiple matches for constant: ", T.unpack v]
            cannotContinue

envInsert :: T.Text -> NGLType -> TypeMSt ()
envInsert v t = modify $ second (Map.insert v t)

check_assignment :: Maybe NGLType -> Maybe NGLType -> TypeMSt ()
check_assignment _ (Just NGLVoid) = errorInLine "Assigning void value to variable"
check_assignment Nothing _ = return ()
check_assignment a b = when (a /= b)
        (errorInLine $ T.concat ["Assigning type ", showType b, " to a variable that has type ", showType a])
    where
        showType = T.pack . show . fromJust

nglTypeOf :: Expression -> TypeMSt (Maybe NGLType)
nglTypeOf (FunctionCall f arg args b) = inferBlock f b *> checkFuncKwArgs f args *> checkFuncUnnamed f arg
nglTypeOf (MethodCall m self arg args) = checkmethodcall m self arg args
nglTypeOf (Lookup mt (Variable v)) = envLookup mt v
nglTypeOf (BuiltinConstant (Variable v)) = return (typeOfConstant v)
nglTypeOf (ConstStr _) = return (Just NGLString)
nglTypeOf (ConstInt _) = return (Just NGLInteger)
nglTypeOf (ConstDouble _) = return (Just NGLDouble)
nglTypeOf (ConstBool _) = return (Just NGLBool)
nglTypeOf (ConstSymbol _) = return (Just NGLSymbol)
nglTypeOf e@(ListExpression _) = do
    mt <- checkindexable e
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

typeOfConstant :: T.Text -> Maybe NGLType
typeOfConstant "STDIN"        = Just NGLString
typeOfConstant "STDOUT"       = Just NGLString
typeOfConstant _              = Nothing

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
typeOfObject NGOSequenceSet{} = Just NGLSequenceSet
typeOfObject NGOCounts{} = Just NGLCounts
typeOfObject NGOVoid = Just NGLVoid
typeOfObject (NGOList []) = Nothing
typeOfObject (NGOList (v:_)) = NGList <$> typeOfObject v


checkuop UOpLen e = checkindexable e *> return (Just NGLInteger)
checkuop UOpMinus e = checknum e
checkuop UOpNot e = checkBool e

checkbop :: BOp -> Expression -> Expression -> TypeMSt (Maybe NGLType)
checkbop BOpAdd a b = do
    t <- liftM2 (<|>)
        (softCheckPair NGLInteger a b)
        (softCheckPair NGLString a b)
    when (isNothing t) $
        errorInLineC ["Addition operator (+) must be applied to a pair of strings or integers"]
    return t
checkbop BOpMul a b = checknum a *> checknum b

checkbop BOpGT  a b = checknum a *> checknum b $> Just NGLBool
checkbop BOpGTE a b = checknum a *> checknum b $> Just NGLBool
checkbop BOpLT  a b = checknum a *> checknum b $> Just NGLBool
checkbop BOpLTE a b = checknum a *> checknum b $> Just NGLBool
checkbop BOpPathAppend a b = softCheck NGLString a *> softCheck NGLString b $> Just NGLString
checkbop BOpNEQ  a b = checkbop BOpEQ a b
checkbop BOpEQ  a b = do
    t <- liftM3 (\x y z -> x <|> y <|> z)
        (softCheckPair NGLInteger a b)
        (softCheckPair NGLDouble a b)
        (softCheckPair NGLString a b)
    when (isNothing t) $
        errorInLineC ["Comparison operators (== or !=) must be applied to a pair of strings or numbers"]
    return (Just NGLBool)


softCheck :: NGLType -> Expression -> TypeMSt (Maybe NGLType)
softCheck expected expr = do
    t <- nglTypeOf expr
    return $ if t /= Just expected
        then Nothing
        else t

softCheckPair t a b = do
    ta <- softCheck t a
    tb <- softCheck t b
    return $! if ta == tb && tb == Just t
        then ta
        else Nothing

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

checknum e = do
    t <- nglTypeOf e
    if t `elem` [Just NGLInteger, Just NGLDouble, Nothing]
        then return t
        else do
            errorInLineC ["Expected numeric expression, got ", show t, " for expression ", show e]
            return $ Just NGLDouble -- a decent guess most of the time

checkindex expr index = checkindex' index *> checkindexable expr
    where
        checkindex' (IndexOne e) = checkinteger e
        checkindex' (IndexTwo a b) = checkinteger' a *> checkinteger' b
        checkinteger' Nothing = return Nothing
        checkinteger' (Just v) = checkinteger v

checkindexable (ListExpression []) = return (Just NGLVoid)
checkindexable (ListExpression es) = do
    types <- nglTypeOf `mapM` es
    let ts = catMaybes types
    if null ts
        then return Nothing
        else do
            unless (allSame ts)
                (errorInLine "List of mixed type")
            return (Just $ head ts)
checkindexable expr = do
    t <- nglTypeOf expr
    case t of
        Just (NGList !btype) -> return $ Just btype
        Just NGLRead -> return t
        e -> do
            errorInLineC ["List expected. Type ", show e , " provided."]
            return $ Just NGLVoid

allFunctions = (builtinFunctions ++) <$> moduleFunctions
moduleFunctions = asks (concatMap modFunctions)

funcInfo fn = do
    fs <- allFunctions
    let matched = filter ((==fn) . funcName) fs
    case matched of
        [fi] -> return fi
        [] -> do
            errorInLineC ["Unknown function '", show fn, "'. ", T.unpack (suggestionMessage (unwrapFuncName fn) (unwrapFuncName . funcName <$> fs))]
            cannotContinue
        _ -> do
            errorInLineC ["Too many matches for function '", show fn, "'"]
            cannotContinue

findMethodInfo :: MethodName -> Expression -> TypeMSt MethodInfo
findMethodInfo m self =  case filter ((==m) . methodName) builtinMethods of
    [mi] -> return mi
    ms@(_:_) -> nglTypeOf self >>= \case
        Nothing -> do
            errorInLineC ["Cannot disambiguate method `", T.unpack (unwrapMethodName m), "` as it is called on an expression of unknown type (", show self, ")."]
            cannotContinue
        Just selfType -> case filter (\mi -> methodSelfType mi == selfType) ms of
            [mi] -> return mi
            _ -> do
                errorInLineC ["Cannot disambiguate method `", T.unpack (unwrapMethodName m), "` as it was called on an unsupported type"]
                cannotContinue
    _ -> do
        errorInLineC
            ["Cannot find method `", T.unpack (unwrapMethodName m), "`. "
            ,T.unpack $ suggestionMessage (unwrapMethodName m) ((unwrapMethodName . methodName) <$> builtinMethods)
            ]
        cannotContinue

checkFuncUnnamed :: FuncName -> Expression -> TypeMSt (Maybe NGLType)
checkFuncUnnamed f arg = do
        targ <- nglTypeOf arg
        Function _ metype _ rtype _ allowAutoComp _ <- funcInfo f
        case metype of
            Just etype -> case targ of
                Just (NGList t)
                    | allowAutoComp -> checkfunctype etype t $> Just (NGList rtype)
                Just t -> checkfunctype etype t $> Just rtype
                Nothing -> do
                    errorInLineC ["While checking types for function ", show f, ".\n\tCould not infer type of argument (saw :", show arg, ")"]
                    cannotContinue
            Nothing -> return Nothing
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
    Function _ _ _ _ argInfo _ _ <- funcInfo f
    mapM_ (check1arg (concat ["function '", show f, "'"]) argInfo) args


check1arg :: String -> [ArgInformation] -> (Variable, Expression) -> TypeMSt ()
check1arg ferr arginfo (Variable v, e) = do
    eType <- nglTypeOf e
    let ainfo = find ((==v) . argName) arginfo
    case (ainfo,eType) of
        (Nothing, _) -> errorInLineC $
            ["Bad argument '", T.unpack v, "' for ",  ferr, ".\n"
            ,T.unpack $ suggestionMessage v (argName <$> arginfo)
            ,"\nThis function takes the following arguments:\n"]
            ++ map ((\aname -> "\t"++aname++"\n") . T.unpack . argName) arginfo
        (_, Nothing) -> return () -- Could not infer type of argument. Maybe an error, but maybe not
        (Just ainfo', Just t') ->
            when (argType ainfo' /= t') $
                errorInLineC
                    ["Bad argument type in ", ferr ,", variable " , show v, ". ",
                    "Expected ", show . argType $ ainfo', " got ", show t', "."]


requireType :: NGLType -> Expression -> TypeMSt NGLType
requireType def_t e = nglTypeOf e >>= \case
    Nothing -> do
        errorInLineC ["Could not infer required type of expression (", show e, ")"]
        return def_t
    Just t -> return t

checkmethodcall :: MethodName -> Expression -> Maybe Expression -> [(Variable, Expression)] -> TypeMSt (Maybe NGLType)
checkmethodcall m self arg args = do
    minfo <- findMethodInfo m self
    let reqSelfType = methodSelfType minfo
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

    let ainfo = methodKwargsInfo minfo
    forM_ args (check1arg (concat ["method '", show m, "'"]) ainfo)
    forM_ (filter argRequired ainfo) $ \ai ->
        case filter (\(Variable v,_) -> v == argName ai) args of
            [_] -> return ()
            [] -> errorInLineC ["Required argument ", T.unpack (argName ai), " is missing in method call ", show m, "."]
            _ -> error "This should never happen: multiple arguments with the same name should have been caught before"
    return . Just . methodReturnType $ minfo

addTypes :: TypeMap -> [(Int, Expression)] -> NGLess [(Int,Expression)]
addTypes tmap exprs = mapM (secondM (runNGLess . recursiveTransform addTypes')) exprs
    where
        addTypes' :: Expression -> NGLess Expression
        addTypes' (Lookup Nothing v@(Variable n)) = case Map.lookup n tmap of
            t@(Just _) -> return $ Lookup t v
            Nothing -> throwScriptError ("Could not assign type to variable '" ++ show n ++ "'.")
        addTypes' e = return e
