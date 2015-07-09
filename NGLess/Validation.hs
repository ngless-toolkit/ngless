{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Validation
    ( validate
    ) where

import Language

import Data.Maybe
import qualified Data.Text as T


validate :: Script -> Either T.Text Script
validate expr = case errors of
        [] -> Right expr
        _ -> Left (T.concat errors)
    where
        errors = catMaybes (map ($expr) checks)
        checks =
            [validate_types
            ,validate_version
            ,validate_pure_function
            ,validate_req_function_args -- check for the existence of required arguments in functions.
            ,validate_val_function_args 
            ,validate_STDIN_only_used_once
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

-- symbols that can be used directly
symbols :: [T.Text]
symbols = ["union", "intersection_strict", "intersection_non_empty", "allow", "deny", "yes", "no", "csv", "tsv", "bam", "sam"]

-- symbols that can be used inside a list.
symbols_list :: [T.Text]
symbols_list = ["gene", "cds", "exon"]


validate_version :: Script -> Maybe T.Text
validate_version sc = nglVersion sc >>= \case
    "0.0" -> Nothing
    version -> Just (T.concat ["Version ", version, " is not supported (only version 0.0 is available)."])

validate_types :: Script -> Maybe T.Text
validate_types (Script _ es) = check_toplevel validate_types' es
    where validate_types' (Assignment _ e@(ConstSymbol _)) = validate_symbol symbols e
          validate_types' (Assignment _ (ListExpression e@[ConstSymbol _])) = errors_from_list $ map (validate_symbol symbols_list) e
          validate_types' _ = Nothing

validate_symbol :: [T.Text] -> Expression -> Maybe T.Text
validate_symbol s (ConstSymbol k)
    | elem k s = Nothing
    | otherwise = Just (T.concat ["Used symbol `", k, "` but possible symbols are: ", T.pack . show $ s])
validate_symbol _ _ = Nothing


-- | check whether results of calling pure functions are use
validate_pure_function (Script _ es) = check_toplevel validate_pure_function' es
    where
        validate_pure_function' (FunctionCall f _ _ _)
            | f `elem` pureFunctions = Just (T.concat ["Result of call function ", T.pack . show $ f, " should be assigned to something."])
        validate_pure_function' _ = Nothing
        pureFunctions =
                    [ Funique
                    , Fsubstrim
                    , Fmap
                    , Fcount
                    , Fas_reads
                    , Fselect
                    ]

validate_req_function_args :: Script -> Maybe T.Text
validate_req_function_args (Script _ es) = check_toplevel validate_req_function_args' es
    where
        validate_req_function_args' (Assignment  _ fc) = validate_req_function_args' fc
        validate_req_function_args' (FunctionCall f _ args _) = has_required_args f args
        validate_req_function_args' _ = Nothing

has_required_args :: FuncName -> [(Variable, Expression)] -> Maybe T.Text
has_required_args f args = errors_from_list $ map has1 (function_required_args f)
    where
        used = map (\(Variable k, _) -> k) args
        has1 a = if a `elem` used
                then Nothing
                else Just (T.concat ["Function ", T.pack . show $ f, " requires argument ", a, "."])

validate_val_function_args :: Script -> Maybe T.Text
validate_val_function_args (Script _ es) = check_toplevel validate_val_function_args' es
    where
        validate_val_function_args' (Assignment  _ fc) = validate_val_function_args' fc
        validate_val_function_args' (FunctionCall f _ args _) = check_symbol_val_in_arg f args
        validate_val_function_args' _ = Nothing



validate_STDIN_only_used_once :: Script -> Maybe T.Text
validate_STDIN_only_used_once (Script _ code) = check_use Nothing code
    where
        check_use _ [] = Nothing
        check_use ub@(Just p) ((lno,e):es)
            | stdin_used e = Just . T.pack . concat $ ["Error on line ", show lno, " STDIN can only be used once in the script (previously used on line ", show p, ")"]
            | otherwise = check_use ub es
        check_use Nothing ((lno,e):es) = check_use (if stdin_used e then Just lno else Nothing) es
        stdin_used (BuiltinConstant (Variable "STDIN")) = True
        stdin_used (ListExpression es) = stdin_used `any` es
        stdin_used (UnaryOp _ e) = stdin_used e
        stdin_used (BinaryOp _ a b) = stdin_used a || stdin_used b
        stdin_used (Condition a b c) = stdin_used a || stdin_used b || stdin_used c
        stdin_used (IndexExpression a ix) = stdin_used a || stdin_used_ix ix
        stdin_used (Assignment _ e) = stdin_used e
        stdin_used (FunctionCall _ e args b) = stdin_used e || stdin_used `any` [e' | (_,e') <- args] || stdin_used_block b
        stdin_used (Sequence es) = stdin_used `any` es
        stdin_used _ = False
        stdin_used_ix (IndexOne a) = stdin_used a
        stdin_used_ix (IndexTwo a b) = stdin_used_maybe a || stdin_used_maybe b
        stdin_used_maybe (Just e) = stdin_used e
        stdin_used_maybe Nothing = False
        stdin_used_block (Just (Block _ e)) = stdin_used e
        stdin_used_block _ = False

check_symbol_val_in_arg :: FuncName -> [(Variable, Expression)]-> Maybe T.Text
check_symbol_val_in_arg f args = errors_from_list $ map check1 args
    where
        allowed = function_args_allowed_symbols f
        check1 (Variable v, expr) = case expr of
            ConstSymbol s       -> if s `elem` (allowed v)
                                    then Nothing
                                    else Just (T.concat ["Argument: `", v, "` expects one of ", T.pack . show $ allowed v, " but got `", s, "`"])
            ListExpression es   -> errors_from_list $ map (\e -> check1 (Variable v, e)) es
            _                   -> Nothing

check_toplevel :: (Expression -> Maybe T.Text) -> [(Int, Expression)] -> Maybe T.Text
check_toplevel _ [] = Nothing
check_toplevel f ((lno,e):es) = case f e of
        Nothing -> check_toplevel f es
        Just m -> Just (T.concat ["Line ", T.pack (show lno), ": ", m])

errors_from_list :: [Maybe T.Text] -> Maybe T.Text
errors_from_list = listToMaybe . catMaybes 
