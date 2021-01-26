{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}

module ValidationIO
    ( validateIO
    ) where

import System.Directory
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import qualified Data.Text as T
import           Control.Monad.Extra (whenJust, whenM)

import NGLess
import Output
import Modules
import Language
import BuiltinFunctions
import FileManagement
import Utils.Suggestion
import ReferenceDatabases
import BuiltinModules.Checks
import Interpretation.Count (executeCountCheck)


-- validation functions live in this Monad, where error messages can be written
type ValidateIO = WriterT [T.Text] (ReaderT [Module] NGLessIO)
liftNGLessIO = lift . lift
tell1 = tell . (:[])

findFunctionIO :: FuncName -> ValidateIO Function
findFunctionIO fname = asks (flip findFunction fname) >>= \case
    Just finfo -> return finfo
    Nothing -> throwShouldNotOccur ("Cannot find information for function: " ++ show fname)

-- | Run as many checks as possible (including non-pure, IO consuming, checks)
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
            ,checkReferencesExist
            ,validateCount
            ]


-- | check that necessary files exist
validateReadInputs :: Script -> ValidateIO ()
validateReadInputs (Script _ es) = checkRecursive validateReadInputs' es
    where
        validateReadInputs' :: Expression -> ValidateIO ()
        validateReadInputs' (FunctionCall f expr args _) = do
            finfo <- findFunctionIO f
            when (ArgCheckFileReadable `elem` funcArgChecks finfo) $
                validateStrVal checkFileReadable' es expr
            forM_ (funcKwArgs finfo) $ \ainfo ->
                when (ArgCheckFileReadable `elem` argChecks ainfo) $
                   validateStrArg checkFileReadable' (argName ainfo) args es
        validateReadInputs' _ = return ()

        checkFileReadable' :: T.Text -> ValidateIO ()
        checkFileReadable' fname =
            (liftNGLessIO . expandPath $ T.unpack fname) >>= \case
                Just p -> liftIO (checkFileReadable p) >>= flip whenJust tell1
                Nothing -> tell1 $ T.concat ["Could not find necessary input file ", fname]


checkReferencesExist :: Script -> ValidateIO ()
checkReferencesExist (Script _ es) = flip checkRecursive es $ \case
        (FunctionCall (FuncName "map") _ args _) -> validateStrArg check1 "reference" args es
        _ -> return ()
    where
        check1 :: T.Text -> ValidateIO ()
        check1 r = do
            mods <- ask
            let refs = concatMap modReferences mods
                ename (ExternalPackagedReference er) = refName er
                ename er = erefName er
                allnames = (ename <$> refs) ++ (refName <$> builtinReferences) ++ mapMaybe refAlias builtinReferences
            unless (r `elem` allnames) $ do
                exists <- liftIO $ doesFileExist (T.unpack r)
                tell1 . T.concat $ [
                            "Could not find reference ", r, " (it is neither built in nor in any of the loaded modules).\n"
                            ] ++ (if exists
                                    then ["\n\tDid you mean to use the argument `fafile` to specify the FASTA file `", r, "`?\n",
                                          "\tmap() uses the argument `reference` for builtin references and `fafile` for a FASTA file path."]
                                    else [])
                            ++ [suggestionMessage r allnames,
                                "\n\tValid options are:"]
                            ++ [T.concat ["\n\t\t - ", v] | v <- allnames]



validateStrArg :: (T.Text -> ValidateIO ()) -> T.Text -> [(Variable,Expression)] -> [(Int,Expression)] -> ValidateIO ()
validateStrArg f v args es = whenJust (lookup (Variable v) args) $ validateStrVal f es

validateStrVal :: (T.Text -> ValidateIO ()) -> [(Int,Expression)] -> Expression -> ValidateIO ()
validateStrVal f _ (ConstStr v) = f v
validateStrVal f es (Lookup _ v) = case tryConstValue v es of
    Just (NGOString t)  -> f t
    _ -> return ()
validateStrVal _ _ _ = return ()

tryConstValue :: Variable -> [(Int,Expression)] -> Maybe NGLessObject
tryConstValue var s = case mapMaybe (getAssignment . snd) s of
        [val] -> Just val
        _ -> Nothing
    where
        getAssignment :: Expression -> Maybe NGLessObject
        getAssignment (Assignment v val) | v == var = staticValue val
        getAssignment _ = Nothing


checkRecursive :: (Expression -> ValidateIO ()) -> [(Int,Expression)] -> ValidateIO ()
checkRecursive f es = forM_ es $ \(lno, e) ->
        censor (addLno lno) (recursiveAnalyse f e)
    where
        addLno lno = map (addLno1 lno)
        addLno1 lno err = T.concat ["Line ", T.pack (show lno), ": ", err]


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
    let ofile' = T.unpack ofile
    whenM (liftIO $ doesFileExist ofile') $
        liftNGLessIO $ outputListLno' WarningOutput ["Writing to file '", ofile', "' will overwrite existing file."]

{- Attempt to run executeCountCheck in the validation stage
 -}
validateCount (Script _ es) = checkRecursive validateCount' es
    where
        validateCount' (FunctionCall (FuncName "count") _ kwargs Nothing) =
            whenJust (constantKWArgs kwargs) (void . liftNGLessIO . executeCountCheck NGOVoid)
        validateCount' _ = return ()
        constantKWArgs :: [(Variable, Expression)] -> Maybe KwArgsValues
        constantKWArgs = mapM $ \(Variable v, e) -> (v,) <$> staticValue e

