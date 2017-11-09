{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Validation
    ( validate
    , uses_STDOUT
    ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Either.Combinators (rightToMaybe)
import           Control.Monad.Extra (whenJust)
import           Control.Monad.Writer.Strict
import           Control.Monad.RWS
import           Control.Monad (foldM_)
import           Data.String.Utils (endswith)
import           Data.List (find)
import           Data.Maybe
import           Data.Char (isUpper)

import Language
import Modules
import NGLess.NGError
import BuiltinFunctions
import Utils.Suggestion

-- | Returns either an error message if it finds any errors or the input script unscathed
validate :: [Module] -> Script -> NGLess Script
validate mods expr = case errors of
        [] -> Right expr
        _ -> throwScriptError . T.unpack . T.concat $ errors
    where
        errors = mapMaybe (\f -> f mods expr) checks ++
                concatMap (\f -> execWriter (f mods expr)) checks'
        checks =
            [validateVersion
            ,validate_variables
            ,validateFunctionReqArgs -- check for the existence of required arguments in functions.
            ,validate_symbol_in_args
            ,validate_STDIN_only_used_once
            ,validate_map_ref_input
            ,validateWriteOName
            ]
        -- The interface below is better and the checks above should be
        -- converted to this style:
        checks' :: [[Module] -> Script -> Writer [T.Text] ()]
        checks' =
            [validateNoConstantAssignments
            ,validateNGLessVersionUses
            ,validatePureFunctions
            ]

{- Each checking function has the type
 -
 - Script -> Maybe T.Text
 -
 - If it finds an error, it returns a Just error; otherwise, Nothing.
 -
 - The validate function just runs all checks and either concatenates all the
 - error messages or passes the script unharmed on the Right side.
 -}


validateVersion :: [Module] -> Script -> Maybe T.Text
validateVersion _ sc = nglVersion <$> nglHeader sc >>= \case
    "0.0" -> Nothing
    "0.5" -> Nothing
    "0.6" -> Nothing
    version -> Just (T.concat ["Version ", version, " is not supported (only versions 0.0/0.5/0.6 are available in this release)."])

-- | check whether results of calling pure functions are use
validatePureFunctions mods (Script _ es) =
        forM_ es $ \(lno, expr) -> case expr of
            FunctionCall fname@(FuncName f) _ _ _
                | isPure fname -> tell1lno lno ["Result of calling function `",  f, "` should be assigned to a variable (this function has no effect otherwise)."]
            _ -> return ()

    where
        isPure f = FunctionCheckReturnAssigned `elem` (fromMaybe [] $ funcChecks <$> findFunction mods f)

validateFunctionReqArgs :: [Module] -> Script -> Maybe T.Text
validateFunctionReqArgs mods = checkRecursiveScript validateFunctionReqArgs'
    where
        validateFunctionReqArgs' (FunctionCall f _ args _) = case findFunction mods f of
                Nothing -> Just (T.concat ["Function ", T.pack . show $ f, " not found."])
                Just finfo -> errors_from_list $ map has1 (funcKwArgs finfo)
                    where
                        used = map (\(Variable k, _) -> k) args
                        has1 ainfo = if not (argRequired ainfo) || argName ainfo `elem` used
                            then Nothing
                            else Just (T.concat ["Function ", T.pack . show $ f, " requires argument ", argName ainfo, "."])
        validateFunctionReqArgs' _ = Nothing

validate_variables :: [Module] -> Script -> Maybe T.Text
validate_variables mods (Script _ es) = runChecker $ forM_ es $ \(_,e) -> case e of
        Assignment (Variable v) e' -> do
            vs <- get
            err <- recursiveAnalyse checkVarUsage e'
            put (v:vs)
            return err
        _ -> recursiveAnalyse checkVarUsage e
    where
        runChecker :: RWS () [Maybe T.Text] [T.Text] () -> Maybe T.Text
        runChecker c = errors_from_list . snd . evalRWS c () $ (fst <$> concatMap modConstants mods)
        checkVarUsage :: Expression -> RWS () [Maybe T.Text] [T.Text] ()
        checkVarUsage (Lookup _ (Variable v)) = do
                used <- get
                when (v `notElem` used) $
                    tell [Just (T.concat ["Could not find variable `", T.pack . show $v, "`. ", suggestionMessage v used])]
        checkVarUsage (FunctionCall _ _ _ (Just block)) = do
            vs <- get
            let unVariable (Variable v) = v
                vs' = unVariable <$> blockVariable block
            put (vs' ++ vs)
        checkVarUsage (Assignment (Variable v) _) = do
            vs <- get
            put (v:vs)
        checkVarUsage _ = return ()

validate_symbol_in_args :: [Module] -> Script -> Maybe T.Text
validate_symbol_in_args mods = checkRecursiveScript validate_symbol_in_args'
    where
        validate_symbol_in_args' (FunctionCall f _ args _) = checkFunction f args
        validate_symbol_in_args' (MethodCall m _ arg0 args) = checkMethod m arg0 args
        validate_symbol_in_args' _ = Nothing

        checkFunction :: FuncName -> [(Variable, Expression)]-> Maybe T.Text
        checkFunction f args = case findFunction mods f of
                Nothing -> Just (T.concat ["Function '", T.pack . show $ f, "' not found"])
                Just finfo -> errors_from_list $ map (check1 finfo) args
            where
                check1 finfo (Variable v, expr) = let legal = allowedFunction finfo v in case expr of
                        ConstSymbol s
                            | s `elem` legal -> Nothing
                            | otherwise -> Just . T.concat $ case findSuggestion s legal of
                                    Nothing ->
                                        ["Argument: `", v, "` (for function ", T.pack (show f), ") expects one of ", showA legal, " but got {", s, "}"]
                                    Just (Suggestion valid reason) ->
                                        ["Argument `", v, "` for function ", T.pack (show f), ", got {", s, "}.\n\tDid you mean {", valid, "} (", reason, ")\n\n",
                                        "Legal arguments are: [", showA legal, "]\n"]
                        ListExpression es   -> errors_from_list $ map (\e -> check1 finfo (Variable v, e)) es
                        _                   -> Nothing

        allowedFunction :: Function -> T.Text -> [T.Text]
        allowedFunction finfo v = fromMaybe [] $ do
            argInfo <- find ((==v) . argName) (funcKwArgs finfo)
            ArgCheckSymbol ss <- find (\case { ArgCheckSymbol{} -> True; _ -> False }) (argChecks argInfo)
            return ss

        findMethod :: MethodName -> Maybe MethodInfo
        findMethod m = find ((==m) . methodName) builtinMethods

        allowedMethod minfo v = fromMaybe [] $ do
            argInfo <- find ((==v) . argName) (methodKwargsInfo minfo)
            ArgCheckSymbol ss <- find (\case { ArgCheckSymbol{} -> True; _ -> False}) (argChecks argInfo)
            return ss

        checkMethod m (Just a) args = checkMethod m Nothing ((Variable "__0", a):args)
        checkMethod m Nothing args = case findMethod m of
                Nothing -> Just (T.concat ["Method'", T.pack . show $ m, "' not found"])
                Just minfo -> errors_from_list $ map (check1m minfo) args
            where
                check1m minfo (Variable v, expr) = let legal = allowedMethod minfo v in case expr of
                    ConstSymbol s
                        | s `elem` legal ->  Nothing
                        | otherwise -> Just . T.concat $ case findSuggestion s legal of
                                Nothing ->
                                    (if v /= "__0" then ["Argument `", v, "` "] else ["Unnamed argument "]) ++ ["(for method ", unwrapMethodName m, ") expects one of ", showA legal, " but got {", s, "}"]
                                Just (Suggestion valid reason) ->
                                    (if v /= "__0" then ["Argument `", v, "` "] else ["Unnamed argument "]) ++ ["(for method ", unwrapMethodName m, ") got {", s, "}"] ++
                                        ["\n\tDid you mean {", valid, "} (", reason, ")\n\nAllowed arguments are: [", showA legal, "]"]
                    ListExpression es   -> errors_from_list $ map (\e -> check1m minfo (Variable v, e)) es
                    _                   -> Nothing

        showA [] = ""
        showA [e] = T.concat ["{", e, "}"]
        showA (e:es) = T.concat ["{", e, "}, ", showA es]



validate_map_ref_input :: [Module] -> Script -> Maybe T.Text
validate_map_ref_input _ = checkRecursiveScript validate_map_ref_input'
    where
        validate_map_ref_input' (FunctionCall (FuncName "map") _ args _) =
            case (lookup (Variable "reference") args, lookup (Variable "fafile") args) of
                (Nothing, Nothing) -> Just "Either fafile or reference must be specified in argument to map function"
                (Just _, Just _) -> Just "You cannot specify both fafile and reference in arguments to map function"
                _ -> Nothing
        validate_map_ref_input' _ = Nothing

validateWriteOName :: [Module] -> Script -> Maybe T.Text
validateWriteOName _ = checkRecursiveScript validateWriteOName'
    where
        validateWriteOName' (FunctionCall (FuncName "write") (Lookup (Just t) _) args _) =
            lookup (Variable "oname") args >>= staticValue >>= \case
                NGOString oname -> case lookup (Variable "format") args of
                    Nothing -> checkType t (T.unpack oname)
                    Just _ -> Nothing
                _ -> Nothing
        validateWriteOName' _ = Nothing
        checkType NGLReadSet oname
            | endswith ".fa" oname = Just "Cannot save data in FASTA format."
            | endswith ".fq" oname = Nothing
            | endswith ".fq.gz" oname = Nothing
            | otherwise = Just . T.concat $ ["Cannot determine output format from filename '", T.pack oname, "'"]
        checkType _ _ = Nothing


validate_STDIN_only_used_once :: [Module] -> Script -> Maybe T.Text
validate_STDIN_only_used_once _ (Script _ code) = check_use Nothing code
    where
        check_use _ [] = Nothing
        check_use ub@(Just p) ((lno,e):es)
            | stdin_used e = Just . T.pack . concat $ ["Error on line ", show lno, " STDIN can only be used once in the script (previously used on line ", show p, ")"]
            | otherwise = check_use ub es
        check_use Nothing ((lno,e):es) = check_use (if stdin_used e then Just lno else Nothing) es
        stdin_used = constant_used "STDIN"


constant_used :: T.Text -> Expression -> Bool
constant_used k (BuiltinConstant (Variable k')) = k == k'
constant_used k (ListExpression es) = constant_used k `any` es
constant_used k (UnaryOp _ e) = constant_used k e
constant_used k (BinaryOp _ a b) = constant_used k a || constant_used k b
constant_used k (Condition a b c) = constant_used k a || constant_used k b || constant_used k c
constant_used k (IndexExpression a ix) = constant_used k a || constant_used_ix k ix
constant_used k (Assignment _ e) = constant_used k e
constant_used k (FunctionCall _ e args b) = constant_used k e || constant_used k `any` [e' | (_,e') <- args] || constant_used_block k b
constant_used k (Sequence es) = constant_used k `any` es
constant_used _ _ = False
constant_used_ix k (IndexOne a) = constant_used k a
constant_used_ix k (IndexTwo a b) = constant_used_maybe k a || constant_used_maybe k b
constant_used_maybe k (Just e) = constant_used k e
constant_used_maybe _ Nothing = False
constant_used_block k (Just (Block _ e)) = constant_used k e
constant_used_block _ _ = False

uses_STDOUT :: Expression -> Bool
uses_STDOUT = constant_used "STDOUT"

validateNoConstantAssignments :: [Module] -> Script -> Writer [T.Text] ()
validateNoConstantAssignments mods (Script _ es) = foldM_ checkAssign builtins es
    where
        checkAssign active (lno,e) = case e of
            Assignment (Variable v) _ -> do
                when (v `elem` active) $
                    tell1lno lno ["assignment to constant `", v, "` is illegal."]
                return $ if T.all isUpper v
                            then v:active
                            else active
            _ -> return active
        builtins = ["STDIN", "STDOUT"] ++ (fst <$> concatMap modConstants mods)


check_toplevel :: (Expression -> Maybe T.Text) -> [(Int, Expression)] -> Maybe T.Text
check_toplevel _ [] = Nothing
check_toplevel f ((lno,e):es) = case f e of
        Nothing -> check_toplevel f es
        Just m -> Just (T.concat ["Line ", T.pack (show lno), ": ", m])

checkRecursiveScript :: (Expression -> Maybe T.Text) -> Script -> Maybe T.Text
checkRecursiveScript f (Script _ es) = check_toplevel checkRecursive es
    where
        checkRecursive :: Expression -> Maybe T.Text
        checkRecursive e = case execWriter (recursiveAnalyse f' e) of
                [] -> Nothing
                errors -> Just $ T.concat errors
        f' :: Expression -> Writer [T.Text] ()
        f' e' = whenJust (f e') (tell . (:[]))

errors_from_list :: [Maybe T.Text] -> Maybe T.Text
errors_from_list errs = case catMaybes errs of
    [] -> Nothing
    errs' -> Just (T.concat errs')

tell1lno :: Int -> [T.Text] -> Writer [T.Text] ()
tell1lno lno err = tell [T.concat $ ["Line ", T.pack (show lno), ": "] ++ err]

validateNGLessVersionUses :: [Module] -> Script -> Writer [T.Text] ()
validateNGLessVersionUses mods sc = case nglVersion <$> nglHeader sc of
        Nothing -> return ()
        Just version -> forM_ (nglBody sc) $ \(lno, expr) ->
            recursiveAnalyse (check version lno) expr
    where
        check :: T.Text -> Int -> Expression -> Writer [T.Text] ()
        check version lno f = case f of
            FunctionCall fname@(FuncName fname') _ _ _ -> case minVersionFor fname of
                Just minV
                    | versionLE minV version -> return ()
                    | otherwise ->
                        tell1lno lno ["Function ", fname', " requires ngless version ", T.pack . show $ fst minV, ".", T.pack . show $ snd minV]
                _ -> return ()
            _ -> return ()
        minVersionFor :: FuncName -> Maybe (Int, Int)
        minVersionFor fname = do
            finfo <- findFunction mods fname
            FunctionCheckMinNGLessVersion minV <- flip find (funcChecks finfo) $ \case
                            FunctionCheckMinNGLessVersion{} -> True
                            _ -> False
            return minV
        versionLE (majV, minV) actual = case parseVersion actual of
            Just (aMaj, aMin) -> case aMaj `compare` majV of
                GT -> True
                EQ -> aMin >= minV
                LT -> False
            _ -> False
        parseVersion :: T.Text -> Maybe (Int, Int)
        parseVersion version = do
            (majV, rest) <- rightToMaybe $ T.decimal version
            guard $ not (T.null rest)
            (minV, _) <- rightToMaybe $ T.decimal (T.tail version)
            return (majV, minV)
