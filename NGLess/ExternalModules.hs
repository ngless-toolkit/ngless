{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings, LambdaCase, RecordWildCards #-}

module ExternalModules
    ( loadModule
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Process
import System.Exit
import System.Directory
import System.FilePath.Posix
import Data.Aeson
import Data.Yaml

import Utils.Utils
import Language
import Modules
import Output
import NGLess

data CommandArgument =
        CommandFlag T.Text Bool
        | CommandOption T.Text (Maybe T.Text) [T.Text]
        | CommandInteger T.Text (Maybe Integer)
    deriving (Eq, Show)

instance FromJSON CommandArgument where
    parseJSON = withObject "command argument" $ \o -> do
        atype <- o .: "atype"
        name <- o .: "name"
        case atype of
            "flag" -> do
                def <- o .: "def"
                return (CommandFlag name def)
            "option" -> do
                def <- o .:? "def"
                allowed <- o .: "allowed"
                return (CommandOption name def allowed)
            "int" -> do
                def <- o .:? "def"
                return (CommandInteger name def)
            _ -> fail ("unknown argument type "++atype)

data FileTypeBase = FastqFileSingle | FastqFilePair | FastqFileTriplet | SamFile | BamFile | SamOrBamFile deriving (Eq,Show)

instance FromJSON FileTypeBase where
    parseJSON = withText "filetypebase" $ \case
        "fq1" -> return FastqFileSingle
        "fq2" -> return FastqFilePair
        "fq3" -> return FastqFileTriplet
        "sam" -> return SamFile
        "bam" -> return BamFile
        "sam_or_bam" -> return SamOrBamFile
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
    parseJSON = withObject "command" $ \o ->
        Command
            <$> o .: "nglName"
            <*> o .: "arg0"
            <*> o .: "arg1"
            <*> o .: "additional"
            <*> o .:? "ret"

data ExternalModule = ExternalModule
    { emInfo :: ModInfo
    , command :: Command
    , validate_cmd :: FilePath
    , validate_args :: [String]
    } deriving (Eq, Show)

instance FromJSON ExternalModule where
    parseJSON = withObject "module" $ \o -> do
        checkO <- o .: "check"
        ExternalModule
            <$> (ModInfo <$> o .: "name" <*> o .: "version")
            <*> o .: "command"
            <*> checkO .: "check_cmd"
            <*> ((fromMaybe []) <$> checkO .:? "check_args")

asFunction Command{..} = Function (FuncName nglName) (Just $ asNGLType arg1) (fromMaybe NGLVoid $ asNGLType <$> ret) (map asArgInfo additional) False

asNGLType = asNGLType' . fileTypeBase
asNGLType' FastqFileSingle = NGLReadSet
asNGLType' FastqFilePair = NGLReadSet
asNGLType' FastqFileTriplet = NGLReadSet
asNGLType' SamFile = NGLMappedReadSet
asNGLType' BamFile = NGLMappedReadSet
asNGLType' SamOrBamFile = NGLMappedReadSet

asArgInfo (CommandFlag name _) = ArgInformation name False NGLBool Nothing
asArgInfo (CommandOption name _ allowed) = ArgInformation name False NGLSymbol (Just allowed)
asArgInfo (CommandInteger name _) = ArgInformation name False NGLInteger Nothing

executeCommand :: Command -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCommand cmd input args = do
    paths <- asfilePaths input
    args' <- argsArguments cmd args
    let cmdline = (paths ++ args')
    outputListLno' TraceOutput ("executing command: ":arg0 cmd:cmdline)
    (exitCode, out, err) <- liftIO $
        readProcessWithExitCode (arg0 cmd) cmdline ""
    outputListLno' TraceOutput ["Processing results: (STDOUT=", out, ", STDERR=", err,")"]
    return NGOVoid

asfilePaths (NGOReadSet1 _ fp) = return [fp]
asfilePaths (NGOReadSet2 _ fp1 fp2) = return [fp1, fp2]
asfilePaths (NGOReadSet3 _ fp1 fp2 fp3) = return [fp1, fp2, fp3]
asfilePaths _ = throwShouldNotOccur ("cannot use this type" :: T.Text)

argsArguments cmd args = catMaybes <$> forM (additional cmd) a1
    where
        a1 :: CommandArgument -> NGLessIO (Maybe String)
        a1 (CommandFlag name def) = do
                isSet <- boolOrTypeError "in command module" $ lookupWithDefault (NGOBool def) name args
                return (if isSet then Just ("--"++T.unpack name) else Nothing)
        a1 (CommandOption name def _) = case lookup name args of
                Nothing -> case def of
                    Nothing -> return Nothing
                    Just s -> return . Just $ ("--"++T.unpack name ++"="++T.unpack s)
                Just v -> symbolOrTypeError "in command module" v >>= \s -> return . Just $ ("--"++T.unpack name++"="++T.unpack s)
        a1 (CommandInteger name def) = case lookup name args <|> (NGOInteger <$> def) of
                Nothing -> return Nothing
                Just (NGOInteger v) -> return . Just $ "--"++T.unpack name++"="++show v
                _ -> throwShouldNotOccur ("in command module, int expected" :: T.Text)


asInternalModule :: ExternalModule -> NGLessIO Module
asInternalModule em@ExternalModule{..} = do
    validateModule em
    return Module
        { modInfo = emInfo
        , modConstants = []
        , modFunctions = [asFunction command]
        , runFunction = const (executeCommand command)
        , validateFunction = const (return [])
        }

validateModule :: ExternalModule -> NGLessIO ()
validateModule  ExternalModule{..} = do
    outputListLno' TraceOutput ("Running module validation for module ":show emInfo:" ":validate_cmd:" ":validate_args)
    (exitCode, out, err) <- liftIO $
        readProcessWithExitCode validate_cmd validate_args ""
    case (exitCode,out,err) of
        (ExitSuccess, "", "") -> return ()
        (ExitSuccess, msg, "") -> outputListLno' TraceOutput ["Module OK. information: ", msg]
        (ExitSuccess, mout, merr) -> outputListLno' TraceOutput ["Module OK. information: ", mout, ". Warning: ", merr]
        (ExitFailure code, _,_) -> do
            outputListLno' WarningOutput ["Module loading failed for module ", show emInfo]
            throwSystemError .concat $ ["Error loading module ", show emInfo, "\n\tstdout='", out, "'\n\tstderr='", err, "'"]


findLoad :: T.Text -> NGLessIO ExternalModule
findLoad modname = do
    m <- liftIO $ findLoad' modname
    case m of
        Right m' -> return m'
        Left err -> throwSystemError ("Could not find external module '" ++ T.unpack modname++"'.\nError message was:\n\t"++err)

findLoad' :: T.Text -> IO (Either String ExternalModule)
findLoad' m = do
    let modfile = "Modules" </> T.unpack m <.> "ngm" </> "module" <.> "yaml"
    exist <- doesFileExist modfile
    if not exist
        then return $ Left ("Could not find module file at "++modfile)
        else decodeEither <$> B.readFile modfile

loadModule :: T.Text -> T.Text -> NGLessIO Module
loadModule m _ = asInternalModule =<< findLoad m

