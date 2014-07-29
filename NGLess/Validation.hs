{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
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
symbols = ["union", "intersection-strict", "intersection-nonempty", "allow", "deny", "yes", "no", "csv", "tsv", "bam", "sam"]

-- symbols that can be used inside a list.
symbols_list :: [T.Text]
symbols_list = ["gene", "cds", "exon"]


validate_version :: Script -> Maybe T.Text
validate_version (Script "0.0" _ ) =  Nothing
validate_version (Script version _) = Just (T.concat ["Version ", version, " is not supported (only version 0.0 is available)."])

validate_types :: Script -> Maybe T.Text
validate_types (Script _ es) = check_toplevel validate_types' es
    where validate_types' (Assignment _ e@(ConstSymbol _)) = validate_symbol symbols e
          validate_types' (Assignment _ (ListExpression e@[ConstSymbol _])) = errors_from_list $ map (validate_symbol symbols_list) e
          validate_types' _ = Nothing

validate_symbol :: [T.Text] -> Expression -> Maybe T.Text
validate_symbol s (ConstSymbol k) = case elem k s of
    True  -> Nothing
    False -> Just (T.concat ["Symbol used is: ", k, " but possible symbols are: ", T.pack . show $ s])
validate_symbol _ _ = Nothing


-- | check whether function result of function calls are used
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
                    ]

validate_req_function_args :: Script -> Maybe T.Text
validate_req_function_args (Script _ es) = check_toplevel validate_req_function_args' es
    where
        validate_req_function_args' (Assignment  _ fc) = validate_req_function_args' fc
        validate_req_function_args' (FunctionCall f _ args _) = exist_req_args f args
        validate_req_function_args' _ = Nothing

exist_req_args :: FuncName -> [(Variable, Expression)] -> Maybe T.Text
exist_req_args f a = case f of
        Fmap   -> lookupVar "reference"
        Fwrite -> lookupVar "ofile"
        _      -> Nothing
    where
        lookupVar v = case lookup v eval_vars of
            Just _  -> Nothing
            Nothing -> Just (T.concat ["Function ", T.pack . show $ f, " requires variable ", v, "."])
        eval_vars = map (\(Variable k,e) -> (k, e)) a


validate_val_function_args :: Script -> Maybe T.Text
validate_val_function_args (Script _ es) = check_toplevel validate_val_function_args' es
    where
        validate_val_function_args' (Assignment  _ fc) = validate_val_function_args' fc
        validate_val_function_args' (FunctionCall f _ args _) = check_symbol_val_in_arg f args
        validate_val_function_args' _ = Nothing



check_symbol_val_in_arg :: FuncName -> [(Variable, Expression)]-> Maybe T.Text
check_symbol_val_in_arg f a = case f of
        Fannotate -> errors_from_list $ map (\(k,v) -> check k (get_v k, v))  
                                        [("features"  ,symbols_list)
                                        ,("ambiguity" ,amb_v)
                                        ,("strand"    ,str_v)
                                        ,("mode"      ,mode_v)]
        Fcount    -> check "counts" (get_v "counts", symbols_list)
        Fwrite    -> check "format" (get_v "format", format_v)
        _         -> Nothing
    where 
        amb_v   = ["allow", "deny"]
        str_v   = ["yes", "no"]
        mode_v  = ["union", "intersection-strict", "intersection-nonempty"]
        format_v= ["tsv", "csv", "bam", "sam"]
        get_v k = lookup k $ map (\(Variable x,e) -> (x, e)) a
        check arg (e,values) = 
            case e of
                Nothing -> Nothing
                Just (ConstSymbol x) -> case elem x values of
                    True  -> Nothing
                    False -> Just (T.concat ["Argument: \"", arg, "\" expects one of ", T.pack . show $ values, " but got \"", x, "\""])
                Just (ListExpression es) -> errors_from_list $ map (\s -> check arg (Just s, values)) es
                Just (Lookup _) -> Nothing
                Just err -> Just (T.concat ["Expected a symbol, but got ", T.pack . show $ err])


check_toplevel :: (Expression -> Maybe T.Text) -> [(Int, Expression)] -> Maybe T.Text
check_toplevel _ [] = Nothing
check_toplevel f ((lno,e):es) = case f e of
        Nothing -> check_toplevel f es
        Just m -> Just (T.concat ["Line ", T.pack (show lno), ": ", m])

errors_from_list :: [Maybe T.Text] -> Maybe T.Text
errors_from_list = listToMaybe . catMaybes 
