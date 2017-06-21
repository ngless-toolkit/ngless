{- Copyright 2013-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RecordWildCards, TemplateHaskell, CPP #-}
module Configuration
    ( NGLessConfiguration(..)
    , ColorSetting(..)
    , guessConfiguration
    , initConfiguration
    , versionStr
    , gitHashStr
    , compilationDateStr
    , embeddedStr
    , dateStr
    ) where

import Control.Monad
import System.Environment (getExecutablePath, lookupEnv)
import System.Directory
import System.FilePath
import Data.Maybe
import Development.GitRev (gitHash)
import qualified Data.Text as T
import qualified Data.Configurator as CF

import CmdArgs

versionStr :: String
versionStr = "0.0.0"

dateStr :: String
dateStr = "not released"

gitHashStr :: String
gitHashStr = $(gitHash)

defaultBaseURL :: FilePath
defaultBaseURL = "http://vm-lux.embl.de/~coelho/ngless-data/"

compilationDateStr :: String
compilationDateStr = __DATE__

embeddedStr :: String
#ifdef NO_EMBED_SAMTOOLS_BWA
embeddedStr = "No"
#else
embeddedStr = "Yes"
#endif


-- | ngless configuration options
data NGLessConfiguration = NGLessConfiguration
    { nConfDownloadBaseURL :: FilePath
    , nConfGlobalDataDirectory :: FilePath
    , nConfUserDirectory :: FilePath
    , nConfUserDataDirectory :: FilePath
    , nConfTemporaryDirectory :: FilePath
    , nConfKeepTemporaryFiles :: Bool
    , nConfTrace :: Bool
    , nConfCreateReportDirectory :: Bool
    , nConfReportDirectory :: FilePath
    , nConfColor :: ColorSetting
    , nConfPrintHeader :: Bool
    , nConfSubsample :: Bool
    , nConfArgv :: [T.Text]
    , nConfVerbosity :: Verbosity
    , nConfSearchPath :: [FilePath]
    } deriving (Eq, Show)


-- | Where to save data (user mode)
getDefaultUserNglessDirectory :: IO FilePath
getDefaultUserNglessDirectory = liftM2 fromMaybe
    ((</> ".local/share/ngless") <$> getHomeDirectory)
    (liftM (</> "ngless")  <$> lookupEnv "XDG_DATA_HOME")


-- | This sets the default configuration based on the environment
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
        , nConfCreateReportDirectory = True
        , nConfTemporaryDirectory = tmp
        , nConfKeepTemporaryFiles = False
        , nConfTrace = False
        , nConfReportDirectory = ""
        , nConfColor = AutoColor
        , nConfPrintHeader = True
        , nConfSubsample = False
        , nConfArgv = []
        , nConfVerbosity = Normal
        , nConfSearchPath = []
        }

-- | Update configuration options based on config files
readConfigFiles :: NGLessConfiguration -> [FilePath] -> IO NGLessConfiguration
readConfigFiles NGLessConfiguration{..} cfiles = do
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
    nConfSearchPath' <- CF.lookupDefault nConfSearchPath cp "search-path"
    nConfCreateReportDirectory' <- CF.lookupDefault nConfCreateReportDirectory cp "create-report"
    return NGLessConfiguration
        { nConfDownloadBaseURL = nConfDownloadBaseURL'
        , nConfGlobalDataDirectory = nConfGlobalDataDirectory'
        , nConfUserDirectory = nConfUserDirectory'
        , nConfUserDataDirectory = nConfUserDataDirectory'
        , nConfTemporaryDirectory = nConfTemporaryDirectory'
        , nConfKeepTemporaryFiles = nConfKeepTemporaryFiles'
        , nConfTrace = nConfTrace
        , nConfCreateReportDirectory = nConfCreateReportDirectory'
        , nConfReportDirectory = nConfReportDirectory
        , nConfColor = nConfColor'
        , nConfPrintHeader = nConfPrintHeader'
        , nConfSubsample = nConfSubsample
        , nConfArgv = nConfArgv
        , nConfVerbosity = nConfVerbosity
        , nConfSearchPath = nConfSearchPath'
        }

-- | Configuration is set in 3 steps:
-- 1. guess. sets defaults
-- 2. read configuration files (readConfigFiles)
-- 3. use command line options
initConfiguration :: NGLessArgs -> IO NGLessConfiguration
initConfiguration opts = do
        config <- guessConfiguration
        config' <- readConfigFiles config (case mode opts of
            DefaultMode{config_files = cs} -> cs
            _ -> [])
        return $! updateConfigurationOpts opts config'
    where

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
                html_odir = case (html_report_directory, input) of
                    (Nothing, ScriptFilePath "-") -> "STDIN.output_ngless"
                    (Nothing, ScriptFilePath fpscript) -> fpscript ++ ".output_ngless"
                    (Nothing, InlineScript _ ) -> "INLINE_SCRIPT.output_ngless"
                    (Just html_odir', _) -> html_odir'
                argv = case input of
                    ScriptFilePath f -> f:extraArgs
                    _ -> extraArgs
                searchPath' = if null searchPath
                                    then nConfSearchPath config
                                    else searchPath
            in config
                    { nConfTrace = trace
                    , nConfKeepTemporaryFiles = ktemp
                    , nConfCreateReportDirectory = createReportDirectory
                    , nConfReportDirectory = html_odir
                    , nConfTemporaryDirectory = tmpdir
                    , nConfPrintHeader = nConfPrintHeader config && not no_header && not print_last
                    , nConfSubsample = subsampleMode
                    , nConfArgv = T.pack <$> argv
                    , nConfSearchPath = searchPath'
                    }
        updateConfigurationOptsMode _ config = config

