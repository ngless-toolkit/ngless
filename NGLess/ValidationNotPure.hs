{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module ValidationNotPure
    ( 
        validate_io,
        validate_io'
    ) where

import System.Directory 
import Data.Maybe

import qualified Data.Text as T

import Language
import ReferenceDatabases

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
            [validate_files,
             validate_def_genomes]


-- | check that necessary files exist
validate_files :: Script -> IO (Maybe T.Text)
validate_files (Script _ es) = check_toplevel validate_files' es
    where
        validate_files' (FunctionCall Ffastq (ConstStr x) _ _) = check_can_read_file x
        validate_files' (FunctionCall Ffastq (Lookup   x) _ _) = validateVar check_can_read_file x es
        validate_files' (FunctionCall Fannotate _ args _) = validateArg check_can_read_file "gff" args es
        validate_files' (Assignment _ e) = validate_files' e
        validate_files' _ = return Nothing
 
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
        isAssignToVar v1 (Assignment v2 (ConstStr val)) | v1 == v2 = Just val
        isAssignToVar _  _ = Nothing

check_can_read_file :: T.Text -> IO (Maybe T.Text)
check_can_read_file fname = let fname' = T.unpack fname in do
    r <- doesFileExist fname'
    if not r
        then return $ Just (T.concat ["File `", fname, "` does not exist."])
        else do
            p <- getPermissions fname'
            if readable p
                then return Nothing
                else return $ Just (T.concat ["File `", fname, "` is not readable (permissions problem)."])

isValidRef :: T.Text -> IO (Maybe T.Text)
isValidRef v = do
    r <- doesFileExist (T.unpack v)
    return (if isDefaultReference v || r
        then Nothing
        else Just (T.concat ["Value of argument reference ", v, " is neither a filepath or a default genome."]))



