{- Copyright 2013-2014 NGLess Authors
 - License: MIT
 -}

module ValidationNotPure
    ( validate_io
    ) where

import System.Directory 
import Data.Maybe

import qualified Data.Text as T

import Language
import ReferenceDatabases

validate_io :: Script -> IO (Maybe [T.Text])
validate_io expr = do
        err <- mapM ($expr) checks
        case catMaybes err of
            [] -> return Nothing
            errors -> return (Just errors)
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
        validate_def_genomes' (FunctionCall Fmap _ args _) = validateArg check_reference "reference" args es -- fromJust can be used, since reference is always required and already validated.
        validate_def_genomes' (Assignment _ e) = validate_def_genomes' e
        validate_def_genomes' _ = return Nothing


validateArg :: (T.Text -> IO (Maybe T.Text)) -> T.Text -> [(Variable,Expression)] -> [(Int,Expression)] -> IO (Maybe T.Text)
validateArg f v args es = case lookup (Variable v) args of
        Just (ConstStr x) -> f x
        Just (Lookup   x) -> validateVar f x es
        _                 -> return Nothing

validateVar :: (T.Text -> IO (Maybe T.Text)) -> Variable -> [(Int,Expression)] -> IO (Maybe T.Text)
validateVar f x es = case get_const_val x es of 
            Right (Just (NGOString t))  -> f t
            Left  err       -> return . Just $ err
            _               -> return Nothing


check_toplevel :: (Expression -> IO (Maybe T.Text)) -> [(Int,Expression)] -> IO (Maybe T.Text)
check_toplevel _ [] = return Nothing
check_toplevel f ((lno,e):es) = do
    r <- f e
    case r of
        Nothing -> check_toplevel f es
        Just m ->  return $ Just (T.concat ["Line ", T.pack (show lno), ": ", m])


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

check_reference :: T.Text -> IO (Maybe T.Text)
check_reference v
    | isDefaultReference v' = return Nothing
    | otherwise = do
        r <- doesFileExist v'
        return (if r
            then Nothing
            else Just (T.concat ["Value of argument reference ", v, " is neither a filepath or a default genome."]))
    where v' = T.unpack v



