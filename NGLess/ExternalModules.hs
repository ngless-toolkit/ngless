{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings, LambdaCase, RecordWildCards #-}

module ExternalModules
    ( loadModule
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.List.Utils as LU
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Process
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist, canonicalizePath)
import System.FilePath.Posix
import Data.Aeson
import Data.Yaml
import Data.Default (def)

import Configuration
import Utils.Utils
import Language
import Modules
import Output
import NGLess

data CommandArgument =
        CommandFlag T.Text Bool
        | CommandOption T.Text (Maybe T.Text) [T.Text]
        | CommandInteger T.Text (Maybe Integer)
        | CommandString T.Text (Maybe T.Text) !Bool
    deriving (Eq, Show)

instance FromJSON CommandArgument where
    parseJSON = withObject "command argument" $ \o -> do
        atype <- o .: "atype"
        name <- o .: "name"
        case atype of
            "flag" -> do
                defVal <- o .: "def"
                return (CommandFlag name defVal)
            "option" -> do
                defVal <- o .:? "def"
                allowed <- o .: "allowed"
                return (CommandOption name defVal allowed)
            "int" -> do
                defVal <- o .:? "def"
                return (CommandInteger name defVal)
            "str" -> do
                defVal <- o .:? "def"
                required <- o .: "required"
                return (CommandString name defVal required)
            _ -> fail ("unknown argument type "++atype)

data FileTypeBase =
    FastqFileSingle
    | FastqFilePair
    | FastqFileTriplet
    | SamFile
    | BamFile
    | SamOrBamFile
    | TSVFile
    deriving (Eq,Show)

instance FromJSON FileTypeBase where
    parseJSON = withText "filetypebase" $ \case
        "fq1" -> return FastqFileSingle
        "fq2" -> return FastqFilePair
        "fq3" -> return FastqFileTriplet
        "sam" -> return SamFile
        "bam" -> return BamFile
        "sam_or_bam" -> return SamOrBamFile
        "tsv" -> return TSVFile
        ft -> fail ("unknown file type '"++T.unpack ft++"'")

data FileType = FileType
    { fileTypeBase :: !FileTypeBase
    , canGzip :: !Bool
    , canBzip2 :: !Bool
    } deriving (Eq, Show)

instance FromJSON FileType where
    parseJSON = withObject "filetype" $ \o ->
        FileType
            <$> o .: "filetype"
            <*> o .:? "can_gzip" .!= False
            <*> o .:? "can_bzip2" .!= False

data Command = Command
    { nglName :: T.Text
    , arg0 :: FilePath
    , arg1 :: FileType
    , additional :: [CommandArgument]
    , ret :: Maybe FileType
    } deriving (Eq, Show)

instance FromJSON Command where
    parseJSON = withObject "function" $ \o ->
        Command
            <$> o .: "nglName"
            <*> o .: "arg0"
            <*> o .: "arg1"
            <*> o .: "additional"
            <*> o .:? "ret"

data ExternalModule = ExternalModule
    { emInfo :: ModInfo
    , modulePath :: FilePath
    , command :: Maybe Command
    , initCmd :: Maybe FilePath
    , initArgs :: [String]
    , references :: [ExternalReference]
    , emCitation :: Maybe T.Text
    } deriving (Eq, Show)

instance FromJSON ExternalModule where
    parseJSON = withObject "module" $ \o -> do
        initO <- o .:? "init"
        (initCmd, initArgs) <- case initO of
            Nothing -> return (Nothing, [])
            Just initO -> do
                init_cmd <- initO .: "init_cmd"
                init_args <- initO .:? "init_args" .!= []
                return (init_cmd, init_args)
        references <- o .:? "references" .!= []
        command <- o .:? "function"
        emCitation <- o .:? "citation"
        emInfo <- ModInfo <$> o .: "name" <*> o .: "version"
        let modulePath = undefined
        return ExternalModule{..}

addPathToRep :: FilePath -> ExternalModule -> ExternalModule
addPathToRep mpath m = m { modulePath = mpath, references = map (addPathToRef mpath) (references m) }

addPathToRef :: FilePath -> ExternalReference -> ExternalReference
addPathToRef mpath er@ExternalReference{..} = er
        { faFile = ma faFile
        , gtfFile = ma <$> gtfFile
        , geneMapFile = ma <$> geneMapFile
        }
    where
        ma p
            | isAbsolute p = p
            | otherwise = mpath </> p
addPathToRef _ er = er


asFunction Command{..} = Function (FuncName nglName) (Just $ asNGLType arg1) [] (fromMaybe NGLVoid $ asNGLType <$> ret) (map asArgInfo additional) False

asNGLType = asNGLType' . fileTypeBase
asNGLType' FastqFileSingle = NGLReadSet
asNGLType' FastqFilePair = NGLReadSet
asNGLType' FastqFileTriplet = NGLReadSet
asNGLType' SamFile = NGLMappedReadSet
asNGLType' BamFile = NGLMappedReadSet
asNGLType' SamOrBamFile = NGLMappedReadSet
asNGLType' TSVFile = NGLCounts

asArgInfo (CommandFlag name _) = ArgInformation name False NGLBool []
asArgInfo (CommandOption name _ allowed) = ArgInformation name False NGLSymbol [ArgCheckSymbol allowed]
asArgInfo (CommandInteger name _) = ArgInformation name False NGLInteger []
asArgInfo (CommandString name _ req) = ArgInformation name req NGLString []

{- | Environment to expose to module processes -}
nglessEnv :: FilePath -> NGLessIO [(String,String)]
nglessEnv basedir = liftIO $ do
    env <- getEnvironment
    nglessPath <- getExecutablePath
    return $ ("NGLESS_NGLESS_BIN", nglessPath)
                :("NGLESS_MODULE_DIR", basedir)
                :env

executeCommand :: FilePath -> Command -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCommand basedir cmd input args = do
    paths <- asfilePaths input
    paths' <- liftIO $ mapM canonicalizePath paths
    args' <- argsArguments cmd args
    env <- nglessEnv basedir
    let cmdline = paths' ++ args'
        process = (proc (basedir </> arg0 cmd) cmdline) { env = Just env }
    outputListLno' TraceOutput ["executing command: ", arg0 cmd, " ", LU.join " " cmdline]
    (exitCode, out, err) <- liftIO $
        readCreateProcessWithExitCode process ""
    outputListLno' TraceOutput ["Processing results: (STDOUT=", out, ", STDERR=", err,")"]
    return NGOVoid

asfilePaths :: NGLessObject -> NGLessIO  [FilePath]
asfilePaths (NGOReadSet _ (ReadSet1 _ fp)) = return [fp]
asfilePaths (NGOReadSet _ (ReadSet2 _ fp1 fp2)) = return [fp1, fp2]
asfilePaths (NGOReadSet _ (ReadSet3 _ fp1 fp2 fp3)) = return [fp1, fp2, fp3]
asfilePaths (NGOCounts fp) = return [fp]
asfilePaths invalid = throwShouldNotOccur ("AsFile path got "++show invalid)

argsArguments :: Command -> KwArgsValues -> NGLessIO [String]
argsArguments cmd args = catMaybes <$> forM (additional cmd) a1
    where
        a1 :: CommandArgument -> NGLessIO (Maybe String)
        a1 (CommandFlag name defVal) = do
                isSet <- boolOrTypeError "in command module" $ lookupWithDefault (NGOBool defVal) name args
                return (if isSet then Just ("--"++T.unpack name) else Nothing)
        a1 (CommandOption name defVal _) = case lookup name args of
                Nothing -> case defVal of
                    Nothing -> return Nothing
                    Just s -> return . Just $ ("--"++T.unpack name ++"="++T.unpack s)
                Just v -> symbolOrTypeError "in command module" v >>= \s -> return . Just $ ("--"++T.unpack name++"="++T.unpack s)
        a1 (CommandInteger name defVal) = case lookup name args <|> (NGOInteger <$> defVal) of
                Nothing -> return Nothing
                Just (NGOInteger v) -> return . Just $ "--"++T.unpack name++"="++show v
                _ -> throwShouldNotOccur "in command module, int expected"
        a1 (CommandString name defVal req) = case lookup name args <|> (NGOString <$> defVal) of
                Just (NGOString v) -> return . Just $ "--"++T.unpack name++"="++T.unpack v
                _ -> throwShouldNotOccur "in command module, string expected"

asInternalModule :: ExternalModule -> NGLessIO Module
asInternalModule em@ExternalModule{..} = do
    validateModule em
    return def
        { modInfo = emInfo
        , modCitation = emCitation
        , modReferences = references
        , modFunctions = maybe [] ((:[]) . asFunction) command
        , runFunction = const (maybe (\_ _ -> return NGOVoid) (executeCommand modulePath) command)
        }

validateModule :: ExternalModule -> NGLessIO ()
validateModule  ExternalModule{..} = case initCmd of
    Nothing -> return ()
    Just initCmd' -> do
        outputListLno' TraceOutput ("Running initialization for module ":show emInfo:" ":initCmd':" ":initArgs)
        env <- nglessEnv modulePath
        (exitCode, out, err) <- liftIO $
            readCreateProcessWithExitCode (proc (modulePath </> initCmd') initArgs) { env = Just env } ""
        case (exitCode,out,err) of
            (ExitSuccess, "", "") -> return ()
            (ExitSuccess, msg, "") -> outputListLno' TraceOutput ["Module OK. information: ", msg]
            (ExitSuccess, mout, merr) -> outputListLno' TraceOutput ["Module OK. information: ", mout, ". Warning: ", merr]
            (ExitFailure code, _,_) -> do
                outputListLno' WarningOutput ["Module loading failed for module ", show emInfo]
                throwSystemError .concat $ ["Error loading module ", show emInfo, "\n",
                    "When running the validation command (", initCmd', " with arguments ", show initArgs, ")\n",
                    "\texit code = ", show code,"\n",
                    "\tstdout='", out, "'\n",
                    "\tstderr='", err, "'"]


findFirstM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findFirstM _ [] = return Nothing
findFirstM f (x:xs) = f x >>= \case
    Nothing -> findFirstM f xs
    other -> return other

findLoad :: T.Text -> T.Text -> NGLessIO ExternalModule
findLoad modname version = do
    let modpath' = "Modules" </> T.unpack modname <.> "ngm"
        modpath = modpath' </> T.unpack version
        modfile = "module.yaml"
    globalDir <- globalDataDirectory
    userDir <- userDataDirectory
    found <- flip findFirstM [".", globalDir, userDir] $ \basedir -> do
        let fname = basedir </> modpath </> modfile
        exists <- liftIO $ doesFileExist fname
        outputListLno' TraceOutput ["Looking for module ", T.unpack modname, "at `", fname, if exists then "` and found it." else "` and did not find it."]
        return $! if exists
            then Just (basedir </> modpath)
            else Nothing
    case found of
        Just mdir -> decodeEither <$> liftIO (B.readFile (mdir </> modfile)) >>= \case
                    Right v -> return $ addPathToRep mdir v
                    Left err -> throwSystemError ("Could not load module file "++ mdir </> modfile ++ ". Error was `" ++ err ++ "`")
        Nothing -> do
            others <- forM [".", globalDir, userDir] $ \basedir -> do
                let dname = basedir </> modpath'
                    listDirectory d = filter (`notElem` [".", ".."]) <$> getDirectoryContents d
                exists <- liftIO $ doesDirectoryExist dname
                if not exists
                     then return []
                     else liftIO (listDirectory dname)
            throwSystemError
                ("Could not find external module '" ++ T.unpack modname ++
                    (case concat others of
                        [] -> "'."
                        foundVersions -> "' version " ++ T.unpack version ++ ".\n"
                                        ++ "Please check the version number. I found the following versions:" ++
                                            concat ["\n\t- " ++ show v | v <- uniq foundVersions]))

loadModule :: T.Text -> T.Text -> NGLessIO Module
loadModule m version = asInternalModule =<< findLoad m version

