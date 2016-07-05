{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}
module ValidationIO
    ( validateIO
    ) where

import System.Directory
import System.FilePath (takeDirectory)
import System.IO.Error
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import qualified Data.Text as T
import           Control.Monad.Extra (whenJust)

import NGLess
import Modules
import Language
import BuiltinFunctions
import Utils.Suggestion
import ReferenceDatabases
import BuiltinModules.Checks


type ValidateIO = WriterT [T.Text] (ReaderT [Module] NGLessIO)
tell1 = tell . (:[])

findFunctionIO :: FuncName -> ValidateIO Function
findFunctionIO fname = flip findFunction fname <$> ask >>= \case
    Just finfo -> return finfo
    Nothing -> throwShouldNotOccur ("Cannot find information for function: " ++ show fname)

validateIO :: [Module] -> Script -> NGLessIO (Maybe [T.Text])
validateIO mods sc = do
        err <- runReaderT (execWriterT (mapM ($sc) checks)) mods
        case err of
            [] -> return Nothing
            errors -> return (Just errors)
    where
        checks =
            [validateReadInputs
            ,validateOFile
            ,validate_def_genomes
            ]


-- | check that necessary files exist
validateReadInputs :: Script -> ValidateIO ()
validateReadInputs (Script _ es) = checkRecursive validateReadInputs' es
    where
        validateReadInputs' :: Expression -> ValidateIO ()
        validateReadInputs' (FunctionCall f expr args _) = do
            finfo <- findFunctionIO f
            when (ArgCheckFileReadable `elem` funcArgChecks finfo) $
                validateStrVal checkFileReadable es expr
            forM_ (funcKwArgs finfo) $ \ainfo ->
                when (ArgCheckFileReadable `elem` argChecks ainfo) $
                   validateStrArg checkFileReadable (argName ainfo) args es
        validateReadInputs' _ = return ()

        checkFileReadable :: T.Text -> ValidateIO ()
        checkFileReadable fname = do
            let fname' = T.unpack fname
            r <- liftIO $ doesFileExist fname'
            if not r
                then do
                    existing <- liftIO $ getDirectoryContents (takeDirectory fname')
                                            `catchIOError` (\_ -> return [])
                    tell1 . T.concat $ ["File `", fname, "` does not exist. ", suggestionMessage fname (T.pack <$> existing)]
                else do
                    p <- liftIO $ getPermissions fname'
                    unless (readable p) $
                        tell1 (T.concat ["File `", fname, "` is not readable (permissions problem)."])

validate_def_genomes :: Script -> ValidateIO ()
validate_def_genomes (Script _ es) = checkRecursive validate_def_genomes' es
    where
        validate_def_genomes' (FunctionCall (FuncName "map") _ args _) = validateStrArg check_reference "reference" args es
                                                                            >> validateStrArg check_fafile "fafile" args es
        validate_def_genomes' _ = return ()


validateStrArg :: (T.Text -> ValidateIO ()) -> T.Text -> [(Variable,Expression)] -> [(Int,Expression)] -> ValidateIO ()
validateStrArg f v args es = whenJust (lookup (Variable v) args) $ validateStrVal f es

validateStrVal :: (T.Text -> ValidateIO ()) -> [(Int,Expression)] -> Expression -> ValidateIO ()
validateStrVal f _ (ConstStr v) = f v
validateStrVal f es (Lookup v) = case tryConstValue v es of
    Just (NGOString t)  -> f t
    _ -> return ()
validateStrVal _ _ _ = return ()

tryConstValue :: Variable -> [(Int,Expression)] -> Maybe NGLessObject
tryConstValue var s = case mapMaybe (getAssignment . snd) s of
        [val] -> Just val
        _ -> Nothing
    where
        getAssignment :: Expression -> Maybe NGLessObject
        getAssignment (Assignment v val) | v == var = getConst val
        getAssignment _ = Nothing
        getConst (ConstStr t) = Just $ NGOString t
        getConst (ConstSymbol t) = Just $ NGOSymbol t
        getConst (ConstInt v) = Just $ NGOInteger v
        getConst (ConstDouble v) = Just $ NGODouble v
        getConst (ConstBool b) = Just $ NGOBool b
        getConst _ = Nothing


checkRecursive :: (Expression -> ValidateIO ()) -> [(Int,Expression)] -> ValidateIO ()
checkRecursive f es = forM_ es $ \(lno, e) ->
        censor (addLno lno) (recursiveAnalyse f e)
    where
        addLno lno = map (addLno1 lno)
        addLno1 lno err = T.concat ["Line ", T.pack (show lno), ": ", err]



check_reference :: T.Text -> ValidateIO ()
check_reference r = do
    mods <- ask
    let refs = concatMap modReferences mods
        ename (ExternalPackagedReference er) = refName er
        ename er = erefName er
        allnames = (ename <$> refs) ++ (refName <$> builtinReferences)
    unless (r `elem` allnames) $ do
        exists <- liftIO $ doesFileExist (T.unpack r)
        tell1 . T.concat $ [
                    "Could not find reference ", r, " (it is neither built in nor in any of the loaded modules)."
                    ] ++ (if exists
                            then ["\n\tDid you mean to use the argument `fafile` to specify the FASTA file `", r, "`?\n",
                                  "\tmap() uses the argument `reference` for builtin references and `fafile` for a FASTA file path."]
                            else [])

check_fafile fafile = do
        r <- liftIO $ doesFileExist (T.unpack fafile)
        unless r $
            tell1 (T.concat ["map function expects a file in argument 'fafile', got ", fafile, ", which is not the name of a file."])

validateOFile (Script _ es) = checkRecursive validateOFile' es
    where
        validateOFile' (FunctionCall f expr args _) = do
            finfo <- findFunctionIO f
            when (ArgCheckFileWritable `elem` funcArgChecks finfo) $
                validateStrVal checkOFileV es expr
            forM_ (funcKwArgs finfo) $ \ainfo ->
                when (ArgCheckFileWritable `elem` argChecks ainfo) $
                   validateStrArg checkOFileV (argName ainfo) args es
        validateOFile' _ = return ()

checkOFileV :: T.Text -> ValidateIO ()
checkOFileV ofile = do
    errors <- liftIO (checkOFile ofile)
    whenJust errors tell1

