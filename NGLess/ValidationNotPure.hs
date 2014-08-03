{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module ValidationNotPure
    ( 
        validate_io,
        validate_io'
    ) where

import Language
import UnpackIlluminaGenomes

import System.Directory 

import Data.Maybe
import qualified Data.Text as T

validate_io :: Script -> IO Script
validate_io expr = do
    r <- validate_io' expr 
    case r of
        Left  err -> error . T.unpack $ err
        Right e   -> return e

validate_io' :: Script -> IO (Either T.Text Script)
validate_io' expr = do
    err <- mapM ($expr) checks
    case catMaybes err of
        [] -> return . Right $ expr
        errors -> return . Left . T.concat $ errors
    where
        checks =
            [validate_fp,
             validate_def_genomes]


-- | check whether function result of function calls are used
validate_fp :: Script -> IO (Maybe T.Text)
validate_fp (Script _ es) = check_toplevel validate_fp' es
    where
        validate_fp' (FunctionCall Ffastq (ConstStr x) _ _) = isValidFile x
--        validate_fp' (FunctionCall Fwrite _ args _) = valArgument "ofile" args 
        validate_fp' (FunctionCall Fannotate _ args _) = valArgument "gff" args 
        validate_fp' (Assignment _ e) = validate_fp' e
        validate_fp' _ = return Nothing
 
valArgument :: T.Text -> [(Variable,Expression)] -> IO (Maybe T.Text)
valArgument v args = case lookup (Variable v) args of
        Just (ConstStr x) -> isValidFile x  
        _                 -> return Nothing

validate_def_genomes :: Script -> IO (Maybe T.Text)
validate_def_genomes (Script _ es) = check_toplevel validate_def_genomes' es
    where
        validate_def_genomes' (FunctionCall Fmap _ args _) = isValidRef (fromJust $ lookup "reference" (eval_vars args)) -- fromJust can be used, since reference is always required and already validated.
        validate_def_genomes' (Assignment _ e) = validate_def_genomes' e
        validate_def_genomes' _ = return Nothing
        eval_vars = map (\(Variable k,e) -> (k, e))



check_toplevel :: (Expression -> IO (Maybe T.Text)) -> [(Int,Expression)] -> IO (Maybe T.Text)
check_toplevel _ [] = return Nothing
check_toplevel f ((lno,e):es) = do
    r <- f e
    case r of
        Nothing -> check_toplevel f es
        Just m ->  return $ Just (T.concat ["Line ", T.pack (show lno), ": ", m])


-------------
isValidFile :: T.Text -> IO (Maybe T.Text)
isValidFile x = do
    r <- doesFileExist (T.unpack x)
    case r of
        True  -> return $ Nothing
        False -> return $ Just (T.concat ["File name: ", x, " does not exist."])

isValidRef :: Expression -> IO (Maybe T.Text)
isValidRef x = case x of
    ConstStr v -> isValidRef' v
    _          -> return Nothing
    where
        isValidRef' v = do
            r <- doesFileExist (T.unpack v)
            case isDefaultGenome v || r of
                True  -> return $ Nothing
                False -> return $ Just (T.concat ["Value of argument reference ", v, " is neither a filepath or a default genome."])



