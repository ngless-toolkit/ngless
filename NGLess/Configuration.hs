{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RecordWildCards, CPP #-}
module Configuration
    ( NGLessConfiguration(..)
    , InstallMode(..)
    , ColorSetting(..)
    , nglConfiguration
    , initConfiguration
    , setupTestConfiguration
    , samtoolsBin
    , bwaBin
    , versionStr
    , compilationDateStr
    , dateStr
    , setQuiet
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment (getExecutablePath, lookupEnv)
import System.Directory
import System.FilePath
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Configurator as CF

import NGLess.NGError
import Dependencies.Embedded
import CmdArgs


binaryExtension :: String
#ifdef WINDOWS
binaryExtension = ".exe"
#else
binaryExtension = ""
#endif


versionStr :: String
versionStr = "0.0.0"

dateStr :: String
dateStr = "not released"

compilationDateStr :: String
compilationDateStr = __DATE__

defaultBaseURL :: FilePath
defaultBaseURL = "http://vm-lux.embl.de/~coelho/ngless-data/"

data InstallMode = User | Root deriving (Eq, Show)

data NGLessConfiguration = NGLessConfiguration
    { nConfDownloadBaseURL :: FilePath
    , nConfGlobalDataDirectory :: FilePath
    , nConfUserDirectory :: FilePath
    , nConfUserDataDirectory :: FilePath
    , nConfTemporaryDirectory :: FilePath
    , nConfKeepTemporaryFiles :: Bool
    , nConfTrace :: Bool
    , nConfCreateOutputDirectory :: Bool
    , nConfOutputDirectory :: FilePath
    , nConfColor :: ColorSetting
    , nConfPrintHeader :: Bool
    , nConfSubsample :: Bool
    , nConfArgv :: [T.Text]
    , nConfVerbosity :: Verbosity
    } deriving (Eq, Show)

getDefaultUserNglessDirectory :: IO FilePath
getDefaultUserNglessDirectory = liftM2 fromMaybe
    ((</> ".local/share/ngless") <$> getHomeDirectory)
    (liftM (</> "ngless")  <$> lookupEnv "XDG_DATA_HOME")

guessConfiguration :: IO NGLessConfiguration
guessConfiguration = do
    tmp <- getTemporaryDirectory
    nglessBinDirectory <- takeDirectory <$> getExecutablePath
    defaultUserNglessDirectory <- getDefaultUserNglessDirectory
    return NGLessConfiguration
        { nConfDownloadBaseURL = defaultBaseURL
        , nConfGlobalDataDirectory = nglessBinDirectory </> "../share/ngless/data"
        , nConfUserDirectory = defaultUserNglessDirectory
        , nConfUserDataDirectory = defaultUserNglessDirectory </> "data"
        , nConfCreateOutputDirectory = True
        , nConfTemporaryDirectory = tmp
        , nConfKeepTemporaryFiles = False
        , nConfTrace = False
        , nConfOutputDirectory = ""
        , nConfColor = AutoColor
        , nConfPrintHeader = True
        , nConfSubsample = False
        , nConfArgv = []
        , nConfVerbosity = Normal
        }

updateConfiguration :: NGLessConfiguration -> [FilePath] -> IO NGLessConfiguration
updateConfiguration NGLessConfiguration{..} cfiles = do
    defaultUserConfig1 <- (</> ".config/ngless.conf") <$> getHomeDirectory
    defaultUserConfig2 <- (</> ".ngless.conf") <$> getHomeDirectory
    let configFiles =
                    [CF.Optional defaultUserConfig1
                    ,CF.Optional defaultUserConfig2
                    ,CF.Optional "/etc/ngless.conf"
                    ] ++ map CF.Required cfiles
    cp <- CF.load configFiles
    nConfDownloadBaseURL' <- CF.lookupDefault nConfDownloadBaseURL cp "download-url"
    nConfGlobalDataDirectory' <- CF.lookupDefault nConfGlobalDataDirectory cp "global-data-directory"
    nConfUserDirectory' <- CF.lookupDefault nConfUserDirectory cp "user-directory"
    nConfUserDataDirectory' <- CF.lookupDefault nConfUserDataDirectory cp "user-data-directory"
    nConfTemporaryDirectory' <- CF.lookupDefault nConfTemporaryDirectory cp "temporary-directory"
    nConfKeepTemporaryFiles' <- CF.lookupDefault nConfKeepTemporaryFiles cp "keep-temporary-files"
    nConfColor' <- CF.lookupDefault AutoColor cp "color"
    nConfPrintHeader' <- CF.lookupDefault nConfPrintHeader cp "print-header"
    return NGLessConfiguration
        { nConfDownloadBaseURL = nConfDownloadBaseURL'
        , nConfGlobalDataDirectory = nConfGlobalDataDirectory'
        , nConfUserDirectory = nConfUserDirectory'
        , nConfUserDataDirectory = nConfUserDataDirectory'
        , nConfTemporaryDirectory = nConfTemporaryDirectory'
        , nConfKeepTemporaryFiles = nConfKeepTemporaryFiles'
        , nConfTrace = nConfTrace
        , nConfCreateOutputDirectory = nConfCreateOutputDirectory
        , nConfOutputDirectory = nConfOutputDirectory
        , nConfColor = nConfColor'
        , nConfPrintHeader = nConfPrintHeader'
        , nConfSubsample = nConfSubsample
        , nConfArgv = nConfArgv
        , nConfVerbosity = nConfVerbosity
        }


setupTestConfiguration :: IO ()
setupTestConfiguration = do
    config <- guessConfiguration
    writeIORef nglConfigurationRef $ config { nConfTemporaryDirectory = "testing_tmp_dir", nConfKeepTemporaryFiles = True, nConfVerbosity = Quiet }

initConfiguration :: NGLessArgs -> IO ()
initConfiguration opts = do
    config <- guessConfiguration
    config' <- updateConfiguration config (case mode opts of
        DefaultMode{config_files = cs} -> cs
        _ -> [])
    writeIORef nglConfigurationRef (updateConfigurationOpts opts config')

setQuiet :: NGLessIO ()
setQuiet = do
    c <- nglConfiguration
    liftIO $ writeIORef nglConfigurationRef $ c { nConfVerbosity = Quiet }

updateConfigurationOpts NGLessArgs{..} config =
        updateConfigurationOptsMode mode $
            config
                { nConfColor = fromMaybe (nConfColor config) color
                , nConfVerbosity = if quiet then Quiet else verbosity
                }

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

checkExecutable :: String -> FilePath -> NGLessIO FilePath
checkExecutable name bin = do
    exists <- liftIO $ doesFileExist bin
    unless exists
        (throwSystemError $ concat [name, " binary not found!\n","Expected it at ", bin])
    is_executable <- executable <$> liftIO (getPermissions bin)
    unless is_executable
        (throwSystemError $ concat [name, " binary found at ", bin, ".\nHowever, it is not an executable file!"])
    return bin

canExecute bin = do
    exists <- doesFileExist bin
    if exists
        then executable <$> getPermissions bin
        else return False


binPath :: InstallMode -> NGLessIO FilePath
binPath Root = do
    nglessBinDirectory <- takeDirectory <$> liftIO getExecutablePath
#ifndef WINDOWS
    return (nglessBinDirectory </> "../share/ngless/bin")
#else
    return nglessBinDirectory
#endif
binPath User = ((</> "bin") . nConfUserDirectory) <$> nglConfiguration

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

writeBin :: FilePath -> IO B.ByteString -> NGLessIO FilePath
writeBin fname bindata = do
    userBinPath <- binPath User
    bindata' <- liftIO bindata
    when (B.null bindata') $
        throwSystemError ("Cannot find " ++ fname ++ " on the system and this is a build without embedded dependencies.")
    liftIO $ do
        createDirectoryIfMissing True userBinPath
        let fname' = userBinPath </> fname
        B.writeFile fname' bindata'
        p <- getPermissions fname'
        setPermissions fname' (setOwnerExecutable True p)
        return fname'

findOrCreateBin :: String -> FilePath -> IO B.ByteString -> NGLessIO FilePath
findOrCreateBin envvar fname bindata = liftIO (lookupEnv envvar) >>= \case
    Just bin -> checkExecutable envvar bin
    Nothing -> do
        path <- findBin fname
        maybe (writeBin fname bindata) return path

bwaBin :: NGLessIO FilePath
bwaBin = findOrCreateBin "NGLESS_BWA_BIN" bwaFname bwaData
    where
        bwaFname = "ngless-" ++ versionStr ++ "-bwa" ++ binaryExtension

samtoolsBin :: NGLessIO FilePath
samtoolsBin = findOrCreateBin "NGLESS_SAMTOOLS_BIN" samtoolsFname samtoolsData
    where
        samtoolsFname = "ngless-" ++ versionStr ++ "-samtools" ++ binaryExtension



