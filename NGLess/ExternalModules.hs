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
import Data.List (find)
import Data.Default (def)

import Configuration
import Utils.Utils
import Language
import Modules
import Output
import NGLess


-- | Just ArgInformation with a possible default value
data CommandArgument = CommandArgument
        { cargInfo :: ArgInformation
        , cargDef :: Maybe NGLessObject
        , cargPayload :: Maybe [T.Text]
        }
    deriving (Eq, Show)

instance FromJSON CommandArgument where
    parseJSON = withObject "command argument" $ \o -> do
        argName <- o .: "name"
        argRequired <- o .:? "required" .!= False
        atype <- o .: "atype"
        (argType, cargDef) <- case atype of
            "flag" -> do
                defVal <- o .:? "def"
                return (NGLBool, NGOBool <$> defVal)
            "option" -> do
                defVal <- o .:? "def"
                return (NGLSymbol, NGOSymbol <$> defVal)
            "int" -> do
                defVal <- o .:? "def"
                return (NGLInteger, NGOInteger <$> defVal)
            "str" -> do
                defVal <- o .:? "def"
                return (NGLString, NGOString <$> defVal)
            _ -> fail ("unknown argument type "++atype)
        argChecks <- if atype == "option"
                then do
                    allowed <- o .: "allowed"
                    return [ArgCheckSymbol allowed]
                else return []
        let cargInfo = ArgInformation{..}
        cargPayload <- (Just . (:[]) <$> o .: "when-true") <|> o .:? "when-true"
        return CommandArgument{..}

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
            <*> o .:? "additional" .!= []
            <*> o .:? "ret"

data ExternalModule = ExternalModule
    { emInfo :: ModInfo
    , modulePath :: FilePath
    , initCmd :: Maybe FilePath
    , initArgs :: [String]
    , commands :: [Command]
    , references :: [ExternalReference]
    , emCitation :: Maybe T.Text
    } deriving (Eq, Show)

instance FromJSON ExternalModule where
    parseJSON = withObject "module" $ \o -> do
        initO <- o .:? "init"
        (initCmd, initArgs) <- case initO of
            Nothing -> return (Nothing, [])
            Just initO' -> do
                init_cmd <- initO' .: "init_cmd"
                init_args <- initO' .:? "init_args" .!= []
                return (init_cmd, init_args)
        references <- o .:? "references" .!= []
        commands <- o .:? "functions" .!= []
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


asFunction Command{..} = Function (FuncName nglName) (Just $ asNGLType arg1) [] (fromMaybe NGLVoid $ asNGLType <$> ret) (map cargInfo additional) False

asNGLType = asNGLType' . fileTypeBase
asNGLType' FastqFileSingle = NGLReadSet
asNGLType' FastqFilePair = NGLReadSet
asNGLType' FastqFileTriplet = NGLReadSet
asNGLType' SamFile = NGLMappedReadSet
asNGLType' BamFile = NGLMappedReadSet
asNGLType' SamOrBamFile = NGLMappedReadSet
asNGLType' TSVFile = NGLCounts


{- | Environment to expose to module processes -}
nglessEnv :: FilePath -> NGLessIO [(String,String)]
nglessEnv basedir = liftIO $ do
    env <- getEnvironment
    nglessPath <- getExecutablePath
    return $ ("NGLESS_NGLESS_BIN", nglessPath)
                :("NGLESS_MODULE_DIR", basedir)
                :env

executeCommand :: FilePath -> [Command] -> T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCommand basedir cmds funcname input args = do
    cmd <- maybe
                (throwShouldNotOccur ("Call to undefined function "++T.unpack funcname++"."))
                return
                (find ((== funcname) . nglName) cmds)
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
argsArguments cmd args = concat <$> forM (additional cmd) (\(CommandArgument ai mdef payload) -> a1 mdef ai payload)
    where
        a1 :: Maybe NGLessObject -> ArgInformation -> Maybe [T.Text] -> NGLessIO [String]
        a1 mdef ArgInformation{..} payload = case lookup argName args <|> mdef of
                Nothing
                    | not argRequired -> return []
                    | otherwise -> throwScriptError $ concat ["Missing value for required argument ", T.unpack argName, "."]
                Just v
                    | argType == NGLBool -> do
                        val <- boolOrTypeError "in command module" v
                        return $! if val
                            then fromMaybe ["--" ++ T.unpack argName] (map T.unpack <$> payload)
                            else []
                    | otherwise -> do
                        asStr <- case argType of
                            NGLString -> T.unpack <$> stringOrTypeError "in external module" v
                            NGLSymbol -> T.unpack <$> symbolOrTypeError "in external module" v
                            NGLInteger -> show <$> integerOrTypeError "in external module" v
                            other -> throwShouldNotOccur ("Unexpected type tag in external module " ++ show other)
                        return [concat ["--", T.unpack argName, "=", asStr]]

asInternalModule :: ExternalModule -> NGLessIO Module
asInternalModule em@ExternalModule{..} = do
    validateModule em
    return def
        { modInfo = emInfo
        , modCitation = emCitation
        , modReferences = references
        , modFunctions = map asFunction commands
        , runFunction = executeCommand modulePath commands
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

