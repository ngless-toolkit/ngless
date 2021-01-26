{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RecordWildCards #-}
module Configuration
    ( NGLessConfiguration(..)
    , ColorSetting(..)
    , guessConfiguration
    , initConfiguration
    ) where

import Control.Monad
import System.Environment (getExecutablePath, lookupEnv)
import System.Directory
import System.FilePath
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Configurator as CF

import CmdArgs

defaultBaseURL :: FilePath
defaultBaseURL = "https://ngless.embl.de/resources/"

-- | ngless configuration options
data NGLessConfiguration = NGLessConfiguration
    { nConfDownloadBaseURL :: FilePath
    , nConfGlobalDataDirectory :: FilePath
    , nConfUserDirectory :: FilePath
    , nConfUserDataDirectory :: FilePath
    , nConfTemporaryDirectory :: FilePath
    , nConfKeepTemporaryFiles :: Bool
    , nConfTrace :: Bool
    , nConfStrictThreads :: Bool
    , nConfCreateReportDirectory :: Bool
    , nConfReportDirectory :: FilePath
    , nConfColor :: ColorSetting
    , nConfPrintHeader :: Bool
    , nConfSubsample :: Bool
    , nConfArgv :: [T.Text]
    , nConfVerbosity :: Verbosity
    , nConfSearchPath :: [FilePath]
    , nConfIndexStorePath :: Maybe FilePath
    } deriving (Eq, Show)


-- | Where to save data (user mode)
getDefaultUserNglessDirectory :: IO FilePath
getDefaultUserNglessDirectory = liftM2 fromMaybe
    ((</> ".local/share/ngless") <$> getHomeDirectory)
    (fmap (</> "ngless") <$> lookupEnv "XDG_DATA_HOME")


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
        , nConfStrictThreads = False
        , nConfReportDirectory = ""
        , nConfColor = AutoColor
        , nConfPrintHeader = True
        , nConfSubsample = False
        , nConfArgv = []
        , nConfVerbosity = Normal
        , nConfSearchPath = []
        , nConfIndexStorePath = Nothing
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
    nConfStrictThreads' <- CF.lookupDefault nConfStrictThreads cp "strict-threads"
    nConfColor' <- CF.lookupDefault AutoColor cp "color"
    nConfPrintHeader' <- CF.lookupDefault nConfPrintHeader cp "print-header"
    nConfSearchPath' <- CF.lookupDefault nConfSearchPath cp "search-path"
    nConfCreateReportDirectory' <- CF.lookupDefault nConfCreateReportDirectory cp "create-report"
    nConfIndexStorePath' <- CF.lookup cp "index-path"
    return NGLessConfiguration
        { nConfDownloadBaseURL = nConfDownloadBaseURL'
        , nConfGlobalDataDirectory = nConfGlobalDataDirectory'
        , nConfUserDirectory = nConfUserDirectory'
        , nConfUserDataDirectory = nConfUserDataDirectory'
        , nConfTemporaryDirectory = nConfTemporaryDirectory'
        , nConfKeepTemporaryFiles = nConfKeepTemporaryFiles'
        , nConfTrace = nConfTrace
        , nConfStrictThreads = nConfStrictThreads'
        , nConfCreateReportDirectory = nConfCreateReportDirectory'
        , nConfReportDirectory = nConfReportDirectory
        , nConfColor = nConfColor'
        , nConfPrintHeader = nConfPrintHeader'
        , nConfSubsample = nConfSubsample
        , nConfArgv = nConfArgv
        , nConfVerbosity = nConfVerbosity
        , nConfSearchPath = nConfSearchPath'
        , nConfIndexStorePath = nConfIndexStorePath' <|> nConfIndexStorePath
        }

-- | Configuration is set in 3 steps:
-- 1. 'guessConfiguration' sets defaults based on environment
-- 2. 'readConfigFiles' reads configuration files
-- 3. 'updateConfigurationOpts' uses command line options to update
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
            let nConfTrace' = fromMaybe
                            (nConfTrace config)
                            trace_flag
                strictThreads' = fromMaybe
                            (nConfStrictThreads config)
                            strictThreads
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
                createReportDirectory' = fromMaybe
                                    (nConfCreateReportDirectory config)
                                    createReportDirectory
                indexPath' = indexPath <|> nConfIndexStorePath config
            in config
                    { nConfTrace = nConfTrace'
                    , nConfStrictThreads = strictThreads'
                    , nConfKeepTemporaryFiles = ktemp
                    , nConfCreateReportDirectory = createReportDirectory'
                    , nConfReportDirectory = html_odir
                    , nConfTemporaryDirectory = tmpdir
                    , nConfPrintHeader = nConfPrintHeader config && not no_header && not print_last
                    , nConfSubsample = subsampleMode
                    , nConfArgv = T.pack <$> argv
                    , nConfSearchPath = searchPath'
                    , nConfIndexStorePath = indexPath'
                    }
        updateConfigurationOptsMode _ config = config

