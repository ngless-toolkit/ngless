{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module ValidationNotPure
    ( 
        validate_io
    ) where

import Language
import UnpackIlluminaGenomes

import System.Directory 

import Data.Maybe
import qualified Data.Text as T

validate_io :: Script -> IO Script
validate_io expr = do
    err <- mapM ($expr) checks
    case catMaybes err of
        [] -> return expr
        errors -> error . T.unpack $ T.concat errors
    where
        checks =
            [validate_fp,
             validate_def_genomes]


-- | check whether function result of function calls are used
validate_fp :: Script -> IO (Maybe T.Text)
validate_fp (Script _ es) = check_toplevel validate_fp' es
    where
        validate_fp' (FunctionCall Ffastq (ConstStr x) _ _) = isValidFile x
        validate_fp' (Assignment _ e) = validate_fp' e
        validate_fp' _ = return Nothing
 

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



