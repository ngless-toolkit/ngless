{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}
module ValidationIO
    ( validateIO
    ) where

import System.Directory
import System.FilePath.Posix (takeDirectory)
import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Language
import NGLess
import ReferenceDatabases

validateIO :: Script -> NGLessIO (Maybe [T.Text])
validateIO expr = do
        err <- mapM ($expr) checks
        case concat err of
            [] -> return Nothing
            errors -> return (Just errors)
    where
        checks =
            [validate_files
            ,validate_write_output
            ,validate_def_genomes
            ]


-- | check that necessary files exist
validate_files :: Script -> NGLessIO [T.Text]
validate_files (Script _ es) = check_toplevel validate_files' es
    where
        validate_files' (FunctionCall (FuncName "fastq") f _ _) = check f
        validate_files' (FunctionCall (FuncName "paired") f args _) = (++) <$> check f <*> validateArg check_can_read_file "second" args es
        validate_files' (FunctionCall (FuncName "annotate") _ args _) = validateArg check_can_read_file "gff" args es
        validate_files' (Assignment _ e) = validate_files' e
        validate_files' _ = return []

        check (ConstStr fname) = check_can_read_file fname
        check (Lookup var) = validateVar check_can_read_file var es
        check _ = return []

validate_def_genomes :: Script -> NGLessIO [T.Text]
validate_def_genomes (Script _ es) = check_toplevel validate_def_genomes' es
    where
        validate_def_genomes' (FunctionCall (FuncName "map") _ args _) = validateArg check_reference "reference" args es -- fromJust can be used, since reference is always required and already validated.
        validate_def_genomes' (Assignment _ e) = validate_def_genomes' e
        validate_def_genomes' _ = return []


validateArg :: (T.Text -> NGLessIO [T.Text]) -> T.Text -> [(Variable,Expression)] -> [(Int,Expression)] -> NGLessIO [T.Text]
validateArg f v args es = case lookup (Variable v) args of
        Just (ConstStr x) -> f x
        Just (Lookup   x) -> validateVar f x es
        _                 -> return []

validateVar :: (T.Text -> NGLessIO [T.Text]) -> Variable -> [(Int,Expression)] -> NGLessIO [T.Text]
validateVar f v es = case get_const_val v es of
            Right (Just (NGOString t))  -> f t
            Left  err       -> return [err]
            _               -> return []
    where

get_const_val :: Variable -> [(Int,Expression)] -> Either T.Text (Maybe NGLessObject)
get_const_val var s = do
        let r = mapMaybe (getAssignment . snd) s
        case r of
            [] -> Left (T.concat ["Variable: ", T.pack . show $ var, "was never assigned to a value."])
            [val] -> Right . Just $ val
            _ -> Right Nothing -- do not validate
    where
        getAssignment :: Expression -> Maybe NGLessObject
        getAssignment (Assignment v val) | v == var = getConst val
        getAssignment _ = Nothing
        getConst (ConstStr t) = Just $ NGOString t
        getConst (ConstSymbol t) = Just $ NGOSymbol t
        getConst (ConstNum v) = Just $ NGOInteger v
        getConst (ConstBool b) = Just $ NGOBool b
        getConst _ = Nothing


check_toplevel :: (Expression -> NGLessIO [T.Text]) -> [(Int,Expression)] -> NGLessIO [T.Text]
check_toplevel f es = concat <$> (forM es $ \(lno, e) -> do
    errs <- f e
    return $ map (\err -> T.concat ["Line ", T.pack (show lno), ": ", err]) errs)


check_can_read_file :: T.Text -> NGLessIO [T.Text]
check_can_read_file fname = liftIO $ do
    let fname' = T.unpack fname
    r <- doesFileExist fname'
    if not r
        then return [T.concat ["File `", fname, "` does not exist."]]
        else do
            p <- getPermissions fname'
            return $ if readable p
                then []
                else [T.concat ["File `", fname, "` is not readable (permissions problem)."]]

check_reference :: T.Text -> NGLessIO [T.Text]
check_reference v
    | isDefaultReference v' = return []
    | otherwise = liftIO $ do
        r <- doesFileExist v'
        return $ if r
            then []
            else [T.concat ["Value of argument reference ", v, " is neither a filepath or a default genome."]]
    where v' = T.unpack v

validate_write_output (Script _ es) = check_toplevel validate_write es
    where
        validate_write (FunctionCall (FuncName "write") f args _) = validateArg check_can_write_dir "ofile" args es
        validate_write _ = return []

check_can_write_dir :: T.Text -> NGLessIO [T.Text]
check_can_write_dir ofile = liftIO $ do
    let dirname = takeDirectory (T.unpack ofile)
    exists <- doesDirectoryExist dirname
    if not exists
        then return [T.concat ["write call to file ", ofile, ", but directory ", T.pack dirname, " does not exist."]]
        else do
            canWrite <- writable <$> getPermissions dirname
            return $ if not canWrite
                then [T.concat ["write call to file ", ofile, ", but directory ", T.pack dirname, " is not writable."]]
                else []
