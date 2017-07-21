{-# LANGUAGE DeriveDataTypeable #-}
{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}
module CmdArgs
    ( ColorSetting(..)
    , Verbosity(..)
    , NThreadsOpts(..)
    , NGLessInput(..)
    , NGLessArgs(..)
    , NGLessMode(..)
    , nglessArgs
    ) where

{-| This is a separate module so that Main & Configuration
 - can share these objects. Putting them in Configuration would, however,
 - pollute it as it is imported from everywhere.
 -}

import Options.Applicative
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import qualified Data.Configurator.Types as CF
import qualified Data.Text as T

data Verbosity = Quiet | Normal | Loud
        deriving (Show, Eq, Ord, Enum)
data ColorSetting = AutoColor | NoColor | ForceColor
    deriving (Eq, Show)

instance CF.Configured ColorSetting where
    convert (CF.String "auto") = Just AutoColor
    convert (CF.String "force") = Just ForceColor
    convert (CF.String "none") = Just NoColor
    convert _ = Nothing


data NThreadsOpts = NThreads Int | NThreadsAuto
    deriving (Eq, Show)

instance CF.Configured NThreadsOpts where
    convert (CF.String "auto") = Just NThreadsAuto
    convert (CF.String val) = NThreads <$> (readMaybe $ T.unpack val)
    convert _ = Nothing

data NGLessInput =
        InlineScript String
        | ScriptFilePath FilePath
    deriving (Eq, Show)

data NGLessArgs = NGLessArgs
        { verbosity :: Verbosity
        , quiet :: Bool
        , color :: Maybe ColorSetting
        , mode :: NGLessMode
        } deriving (Eq, Show)
data NGLessMode =
        DefaultMode
              { input :: NGLessInput
              , debug_mode :: String
              , validateOnly :: Bool
              , print_last :: Bool
              , trace_flag :: Maybe Bool
              , nThreads :: NThreadsOpts
              , createReportDirectory :: Bool
              , html_report_directory :: Maybe FilePath
              , temporary_directory :: Maybe FilePath
              , keep_temporary_files :: Maybe Bool
              , config_files :: [FilePath]
              , no_header :: Bool
              , subsampleMode :: Bool
              , searchPath :: [FilePath]
              , extraArgs :: [String]
              }
        | InstallGenMode
              { refname :: T.Text
              }
        | CreateReferencePackMode
              { oname :: FilePath
              , genome_url :: String
              , gtf_url :: Maybe String
              , functional_map_url :: Maybe String
              }
        | DownloadFileMode
              { origUrl :: String
              , localFile :: FilePath
              }
        | DownloadDemoMode
              { demoName :: String
              }
        | PrintPathMode
              { pathDesired :: String
              }
           deriving (Eq, Show)

parseVerbosity = option (eitherReader readVerbosity) (long "verbosity" <> short 'v' <> value Normal)
    where
        readVerbosity :: String -> Either String Verbosity
        readVerbosity "quiet" = Right Quiet
        readVerbosity "normal" = Right Normal
        readVerbosity "full" = Right Loud
        readVerbosity other = Left ("Cannot parse '" ++ other ++ "' as a verbosity")

parseColor = optional $ option (eitherReader readColor) (long "color" <> help colorHelp)
    where
        readColor "auto" = Right AutoColor
        readColor "no" = Right NoColor
        readColor "yes" = Right ForceColor
        readColor "force" = Right ForceColor
        readColor _ = Left "Could not parse color option (valid options are 'auto', 'force', and 'no')"
        colorHelp = "Color settings, one of 'auto' (color if writing to a terminal, this is the default), 'force' (always color), 'no' (no color)."

parseInput :: Parser NGLessInput
parseInput = InlineScript <$> strOption
                        (long "script"
                        <> short 'e'
                        <> help "inline script to execute")
            <|> ScriptFilePath <$> strArgument (metavar "INPUT" <> help "Filename of script to interpret")

parseNThreads = option (eitherReader readNThreads) (long "jobs" <> short 'j' <> value (NThreads 1))
    where
        readNThreads "auto" = Right NThreadsAuto
        readNThreads val = case readMaybe val of
                            Just n -> Right (NThreads n)
                            Nothing -> Left ("Failed to parse "++val++" as a threads option")

mainArgs = DefaultMode
              <$> parseInput -- input :: NGLessInput
              <*> strOption (long "debug" <> value "") -- debug_mode :: String
              <*> switch (long "validate-only" <> short 'n' <> help "Only validate input, do not run script") -- validateOnly :: Bool
              <*> switch (long "print-last" <> short 'p' <> help "print value of last line in script") -- print_last :: Bool
              <*> optional (switch $ long "trace" <> help "Set highest verbosity mode") -- trace_flag :: Maybe Bool
              <*> parseNThreads
              <*> switch (long "create-report" <> help "whether to create the report directory") -- createReportDirectory :: Bool
              <*> optional (strOption $ long "html-report-directory" <> short 'o' <> help "name of output directory") -- html_report_directory :: Maybe FilePath
              <*> optional (strOption $ long "temporary-directory" <> short 't' <> help "Directory where to store temporary files") -- temporary_directory :: Maybe FilePath
              <*> optional (switch $ long "keep-temporary-files" <> help "Whether to keep temporary files (default is delete them)") -- keep_temporary_files :: Maybe Bool
              <*> many (strOption $ long "config-file" <> help "Configuration files to parse") -- config_files :: Maybe [FilePath]
              <*> switch (long "no-header" <> help "Do not print copyright information") -- no_header :: Bool
              <*> switch (long "subsample" <> help "Subsample mode: quickly test a pipeline by discarding 99% of the input")-- subsampleMode :: Bool
              <*> many (strOption $ long "search-dir" <> help "Reference search directories (replace <references> in script)") -- searchPath :: [FilePath]
              <*> many (strArgument (metavar "ARGV")) -- extraArgs :: [String]

installArgs = (flag' InstallGenMode (long "install-reference-data"))
                <*> (T.pack <$> strArgument (help "Name of reference to install" <> metavar "REF"))
        -- += details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

createRefArgs = flag' CreateReferencePackMode (long "create-reference-pack" <> internal)
                        <*> strOption (long "output-name" <> internal)
                        <*> strOption (long "genome-url" <> internal)
                        <*> optional (strOption $ long "gtf-url" <> internal)
                        <*> optional (strOption $ long "functional-map-url" <> internal)
        -- += details ["Example:", "ngless --create-reference-pack ref.tar.gz -g http://...genome.fa.gz -a http://...gtf.fa.gz"]

downloadFileArgs = (flag' DownloadFileMode (long "download-file"))
                        <*> strOption (long "download-url")
                        <*> strOption (long "local-file")

downloadDemoArgs = flag' DownloadDemoMode (long "download-demo")
                        <*> strArgument (metavar "DEMO-NAME")

printPathArgs = flag' PrintPathMode (long "print-path")
                        <*> strArgument (metavar "EXEC")

nglessArgs :: Parser NGLessArgs
nglessArgs = NGLessArgs
                <$> parseVerbosity
                <*> switch (long "quiet" <> short 'q')
                <*> parseColor
                <*> (mainArgs <|> downloadFileArgs <|> downloadDemoArgs <|> installArgs <|> createRefArgs <|> printPathArgs)
