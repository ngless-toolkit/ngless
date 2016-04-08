{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RecordWildCards #-}
module Configuration
    ( NGLessConfiguration(..)
    , InstallMode(..)
    , ColorSetting(..)
    , nglConfiguration
    , nglessDataBaseURL
    , initConfiguration
    , setupTestConfiguration
    , globalDataDirectory
    , userDataDirectory
    , samtoolsBin
    , bwaBin
    , outputDirectory
    , temporaryFileDirectory
    , traceFlag
    , versionStr
    , dateStr
    , setVerbosity
    , getVerbosity
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment (getExecutablePath)
import System.Directory
import System.FilePath.Posix
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Configurator as CF

import NGLess
import Dependencies.Embedded
import CmdArgs

versionStr :: String
versionStr = "0.0.0"

dateStr :: String
dateStr = "not released"

data InstallMode = User | Root deriving (Eq, Show)

data NGLessConfiguration = NGLessConfiguration
    { nConfDownloadBaseURL :: FilePath
    , nConfGlobalDataDirectory :: FilePath
    , nConfUserDirectory :: FilePath
    , nConfTemporaryDirectory :: FilePath
    , nConfKeepTemporaryFiles :: Bool
    , nConfTrace :: Bool
    , nConfCreateOutputDirectory :: Bool
    , nConfOutputDirectory :: FilePath
    , nConfColor :: ColorSetting
    , nConfPrintHeader :: Bool
    , nConfSubsample :: Bool
    , nConfArgv :: [T.Text]
    } deriving (Eq, Show)

verbosityRef :: IORef Verbosity
{-# NOINLINE verbosityRef #-}
verbosityRef = unsafePerformIO (newIORef Normal)

setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef verbosityRef
getVerbosity :: IO Verbosity
getVerbosity = readIORef verbosityRef

guessConfiguration :: IO NGLessConfiguration
guessConfiguration = do
    tmp <- getTemporaryDirectory
    nglessBinDirectory <- takeDirectory <$> getExecutablePath
    defaultUserNglessDirectory <- (</> ".ngless") <$> getHomeDirectory
    return NGLessConfiguration
        { nConfDownloadBaseURL = "http://127.0.0.1/"
        , nConfGlobalDataDirectory = nglessBinDirectory </> "../share/ngless/data"
        , nConfUserDirectory = defaultUserNglessDirectory
        , nConfCreateOutputDirectory = True
        , nConfTemporaryDirectory = tmp
        , nConfKeepTemporaryFiles = False
        , nConfTrace = False
        , nConfOutputDirectory = ""
        , nConfColor = AutoColor
        , nConfPrintHeader = True
        , nConfSubsample = False
        , nConfArgv = []
        }

updateConfiguration :: NGLessConfiguration -> [FilePath] -> IO NGLessConfiguration
updateConfiguration NGLessConfiguration{..} cfiles = do
    defaultUserConfig1 <- (</> ".local/config/ngless.conf") <$> getHomeDirectory
    defaultUserConfig2 <- (</> ".ngless.conf") <$> getHomeDirectory
    let configFiles =
                    [CF.Optional defaultUserConfig1
                    ,CF.Optional defaultUserConfig2
                    ,CF.Optional "/etc/ngless.conf"
                    ] ++ (map CF.Required cfiles)
    cp <- CF.load configFiles
    nConfDownloadBaseURL' <- CF.lookupDefault nConfDownloadBaseURL cp "download-url"
    nConfGlobalDataDirectory' <- CF.lookupDefault nConfGlobalDataDirectory cp "global-data-directory"
    nConfUserDirectory' <- CF.lookupDefault nConfUserDirectory cp "user-directory"
    nConfTemporaryDirectory' <- CF.lookupDefault nConfTemporaryDirectory cp "temporary-directory"
    nConfKeepTemporaryFiles' <- CF.lookupDefault nConfKeepTemporaryFiles cp "keep-temporary-files"
    nConfColor' <- CF.lookupDefault AutoColor cp "color"
    nConfPrintHeader' <- CF.lookupDefault nConfPrintHeader cp "print-header"
    return NGLessConfiguration
        { nConfDownloadBaseURL = nConfDownloadBaseURL'
        , nConfGlobalDataDirectory = nConfGlobalDataDirectory'
        , nConfUserDirectory = nConfUserDirectory'
        , nConfTemporaryDirectory = nConfTemporaryDirectory'
        , nConfKeepTemporaryFiles = nConfKeepTemporaryFiles'
        , nConfTrace = nConfTrace
        , nConfCreateOutputDirectory = nConfCreateOutputDirectory
        , nConfOutputDirectory = nConfOutputDirectory
        , nConfColor = nConfColor'
        , nConfPrintHeader = nConfPrintHeader'
        , nConfSubsample = nConfSubsample
        , nConfArgv = nConfArgv
        }


setupTestConfiguration :: IO ()
setupTestConfiguration = do
    setVerbosity Quiet
    config <- guessConfiguration
    writeIORef nglConfigurationRef $ config { nConfTemporaryDirectory = "testing_tmp_dir", nConfKeepTemporaryFiles = True }

initConfiguration :: NGLessArgs -> IO ()
initConfiguration opts = do
    config <- guessConfiguration
    config' <- updateConfiguration config (case mode opts of
        DefaultMode{config_files = cs} -> cs
        _ -> [])
    writeIORef nglConfigurationRef (updateConfigurationOpts opts config')

updateConfigurationOpts NGLessArgs{..} config =
        updateConfigurationOptsMode mode $
            config { nConfColor = fromMaybe (nConfColor config) color }

updateConfigurationOptsMode DefaultMode{..} config =
    let trace = fromMaybe
                    (nConfTrace config)
                    trace_flag
        ktemp = fromMaybe
                    (nConfKeepTemporaryFiles config)
                    keep_temporary_files
        tmpdir = fromMaybe
                    (nConfTemporaryDirectory config)
                    temporary_directory
        odir = case (output_directory, input) of
            (Nothing, ScriptFilePath "-") -> "STDIN.output_ngless"
            (Nothing, ScriptFilePath fpscript) -> fpscript ++ ".output_ngless"
            (Nothing, InlineScript _ ) -> "INLINE_SCRIPT.output_ngless"
            (Just odir', _) -> odir'
        argv = case input of
            ScriptFilePath f -> f:extraArgs
            _ -> extraArgs
    in config
            { nConfTrace = trace
            , nConfKeepTemporaryFiles = ktemp
            , nConfCreateOutputDirectory = createOutputDirectory
            , nConfOutputDirectory = odir
            , nConfTemporaryDirectory = tmpdir
            , nConfPrintHeader = nConfPrintHeader config && not no_header && not print_last
            , nConfSubsample = subsampleMode
            , nConfArgv = T.pack <$> argv
            }
updateConfigurationOptsMode _ config = config

nglConfigurationRef :: IORef NGLessConfiguration
{-# NOINLINE nglConfigurationRef #-}
nglConfigurationRef = unsafePerformIO (newIORef (error "Configuration not yet set"))

nglConfiguration :: NGLessIO NGLessConfiguration
nglConfiguration = liftIO $ readIORef nglConfigurationRef

outputDirectory :: NGLessIO FilePath
outputDirectory = nConfOutputDirectory <$> nglConfiguration

temporaryFileDirectory :: NGLessIO FilePath
temporaryFileDirectory = nConfTemporaryDirectory <$> nglConfiguration

traceFlag :: NGLessIO Bool
traceFlag = nConfTrace <$> nglConfiguration

nglessDataBaseURL :: NGLessIO FilePath
nglessDataBaseURL = nConfDownloadBaseURL <$> nglConfiguration
globalDataDirectory :: NGLessIO FilePath
globalDataDirectory = nConfGlobalDataDirectory <$> nglConfiguration

userNglessDirectory :: NGLessIO FilePath
userNglessDirectory = nConfUserDirectory <$> nglConfiguration

userDataDirectory :: NGLessIO FilePath
userDataDirectory = (</> "data") <$> userNglessDirectory

check_executable :: String -> FilePath -> IO FilePath
check_executable name bin = do
    exists <- doesFileExist bin
    unless exists
        (error $ concat [name, " binary not found!\n","Expected it at ", bin])
    is_executable <- executable <$> getPermissions bin
    unless is_executable
        (error $ concat [name, " binary found at ", bin, ".\nHowever, it is not an executable file!"])
    return bin

canExecute bin = do
    exists <- doesFileExist bin
    if exists
        then executable <$> getPermissions bin
        else return False


binPath :: InstallMode -> NGLessIO FilePath
binPath Root = do
    nglessBinDirectory <- takeDirectory <$> liftIO getExecutablePath
    return (nglessBinDirectory </> "../share/bin")
binPath User = (</> "bin") <$> userNglessDirectory

findBin :: FilePath -> NGLessIO (Maybe FilePath)
findBin fname = do
    rootPath <- (</> fname) <$> binPath Root
    rootex <- liftIO $ canExecute rootPath
    if rootex then
        return (Just rootPath)
    else do
        userpath <- (</> fname) <$> binPath User
        userex <- liftIO $ canExecute userpath
        return $ if userex
            then Just userpath
            else Nothing

writeBin :: FilePath -> B.ByteString -> NGLessIO FilePath
writeBin fname bindata = do
    userBinPath <- binPath User
    liftIO $ do
        createDirectoryIfMissing True userBinPath
        let fname' = userBinPath </> fname
        B.writeFile fname' bindata
        p <- getPermissions fname'
        setPermissions fname' (setOwnerExecutable True p)
        return fname'

findOrCreateBin :: FilePath -> B.ByteString -> NGLessIO FilePath
findOrCreateBin fname bindata = do
    path <- findBin fname
    maybe (writeBin fname bindata) return path

bwaBin :: NGLessIO FilePath
bwaBin = findOrCreateBin bwaFname =<< liftIO bwaData
    where
        bwaFname = "ngless-" ++ versionStr ++ "-bwa"

samtoolsBin :: NGLessIO FilePath
samtoolsBin = findOrCreateBin samtoolsFname =<< liftIO samtoolsData
    where
        samtoolsFname = "ngless-" ++ versionStr ++ "-samtools"



