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
        validate_fp' (FunctionCall Ffastq (Lookup   x) _ _) = validateVar isValidFile x es
        validate_fp' (FunctionCall Fannotate _ args _) = validateArg isValidFile "gff" args es
        validate_fp' (Assignment _ e) = validate_fp' e
        validate_fp' _ = return Nothing
 
validate_def_genomes :: Script -> IO (Maybe T.Text)
validate_def_genomes (Script _ es) = check_toplevel validate_def_genomes' es
    where
        validate_def_genomes' (FunctionCall Fmap _ args _) = validateArg isValidRef "reference" args es -- fromJust can be used, since reference is always required and already validated.
        validate_def_genomes' (Assignment _ e) = validate_def_genomes' e
        validate_def_genomes' _ = return Nothing


validateArg :: (T.Text -> IO (Maybe T.Text)) -> T.Text -> [(Variable,Expression)] -> [(Int,Expression)] -> IO (Maybe T.Text)
validateArg f v args es = case lookup (Variable v) args of
        Just (ConstStr x) -> f x
        Just (Lookup   x) -> validateVar f x es
        _                 -> return Nothing

validateVar :: (T.Text -> IO (Maybe T.Text)) -> Variable -> [(Int,Expression)] -> IO (Maybe T.Text)
validateVar f x es = case get_const_val x es of 
            Left  err -> return . Just $ err
            Right v   -> if isNothing v then return Nothing else f . fromJust $ v


check_toplevel :: (Expression -> IO (Maybe T.Text)) -> [(Int,Expression)] -> IO (Maybe T.Text)
check_toplevel _ [] = return Nothing
check_toplevel f ((lno,e):es) = do
    r <- f e
    case r of
        Nothing -> check_toplevel f es
        Just m ->  return $ Just (T.concat ["Line ", T.pack (show lno), ": ", m])


get_const_val :: Variable -> [(Int,Expression)] -> Either T.Text (Maybe T.Text)
get_const_val v s = do
        let r = mapMaybe (\(_,e') -> isAssignToVar v e') s
        case length r of
            0 -> Left (T.concat ["Variable: ", T.pack . show $ v, "was never assigned to a value."])
            1 -> Right . Just $ head r
            _ -> Right Nothing -- do not validate
    where 
        isAssignToVar :: Variable -> Expression -> Maybe T.Text
        isAssignToVar v1 (Assignment v2 (ConstStr val)) = if v1 == v2 then Just val else Nothing
        isAssignToVar _  _ = Nothing

-------------
isValidFile :: T.Text -> IO (Maybe T.Text)
isValidFile x = do
    r <- doesFileExist (T.unpack x)
    case r of
        True  -> return $ Nothing
        False -> return $ Just (T.concat ["File name: ", x, " does not exist."])

isValidRef :: T.Text -> IO (Maybe T.Text)
isValidRef v = do
    r <- doesFileExist (T.unpack v)
    case isDefaultGenome v || r of
        True  -> return $ Nothing
        False -> return $ Just (T.concat ["Value of argument reference ", v, " is neither a filepath or a default genome."])



