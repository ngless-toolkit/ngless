{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
module CmdArgs
    ( ColorSetting(..)
    , Verbosity(..)
    , NGLessArgs(..)
    , NGLessMode(..)
    , nglessArgs
    ) where

{-| This is a separate module so that Main & Configuration
 - can share these objects. Putting them in Configuration would, however,
 - pollute it as it is imported from everywhere.
 -}

import Options.Applicative
import qualified Data.Configurator.Types as CF

data Verbosity = Quiet | Normal | Loud
        deriving (Show, Eq, Ord, Enum)
data ColorSetting = AutoColor | NoColor | ForceColor
    deriving (Eq, Show)

instance CF.Configured ColorSetting where
    convert (CF.String "auto") = Just AutoColor
    convert (CF.String "force") = Just ForceColor
    convert (CF.String "none") = Just NoColor
    convert _ = Nothing

data NGLessArgs = NGLessArgs
        { verbosity :: Verbosity
        , quiet :: Bool
        , color :: Maybe ColorSetting
        , mode :: NGLessMode
        } deriving (Eq, Show)
data NGLessMode =
        DefaultMode
              { input :: String
              , debug_mode :: String
              , script :: Maybe String
              , print_last :: Bool
              , trace_flag :: Maybe Bool
              , nThreads :: Int
              , createOutputDirectory :: Bool
              , output_directory :: Maybe FilePath
              , temporary_directory :: Maybe FilePath
              , keep_temporary_files :: Maybe Bool
              , config_files :: [FilePath]
              , no_header :: Bool
              , subsampleMode :: Bool
              , extraArgs :: [String]
              }
        | InstallGenMode
              { input :: String
              }
        | CreateReferencePackMode
              { oname :: FilePath
              , genome_url :: String
              , gtf_url :: String
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
        readColor _ = Left "Could not parse color option"
        colorHelp = "Color settings, one of 'auto' (color if writing to a terminal, this is the default), 'force' (always color), 'no' (no color)."

mainArgs = DefaultMode
              <$> strArgument (metavar "INPUT" <> value "")-- input :: String
              <*> strOption (long "debug" <> value "") -- debug_mode :: String
              <*> optional (strOption (long "script" <> short 'e')) -- script :: Maybe String
              <*> switch (long "print-last" <> short 'p' <> help "print value of last line in script") -- print_last :: Bool
              <*> optional (switch (long "trace")) -- trace_flag :: Maybe Bool
              <*> option auto (long "jobs" <> short 'j' <> value 1) -- nThreads :: Int
              <*> switch (long "output" <> help "whether to create the ngless_output directory") -- createOutputDirectory :: Bool
              <*> optional (strOption $ long "output-directory" <> short 'o' <> help "name of output directory") -- output_directory :: Maybe FilePath
              <*> optional (strOption $ long "temporary-directory" <> short 't' <> help "Directory where to store temporary files") -- temporary_directory :: Maybe FilePath
              <*> optional (switch $ long "keep-temporary-files" <> help "Whether to keep temporary files (default is delete them)") -- keep_temporary_files :: Maybe Bool
              <*> many (strOption $ long "config-file" <> help "Configuration files to parse") -- config_files :: Maybe [FilePath]
              <*> switch (long "no-header" <> help "Do not print copyright information") -- no_header :: Bool
              <*> switch (long "subsample" <> help "Subsample mode: quickly test a pipeline by discarding 99% of the input")-- subsampleMode :: Bool
              <*> many (strArgument (metavar "ARGV")) -- extraArgs :: [String]

installArgs = (flag' InstallGenMode (long "install-reference-data"))
                <*> strOption (long "reference")
        -- += details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

createRefArgs = (flag' CreateReferencePackMode (long "create-reference-pack"))
                        <*> strOption (long "output-name")
                        <*> strOption (long "genome-url")
                        <*> strOption (long "gtf-url")
        -- += details ["Example:", "ngless --create-reference-pack ref.tar.gz -g http://...genome.fa.gz -a http://...gtf.fa.gz"]


nglessArgs :: Parser NGLessArgs
nglessArgs = NGLessArgs
                <$> parseVerbosity
                <*> switch (long "quiet" <> short 'q')
                <*> parseColor
                <*> (mainArgs <|> installArgs <|> createRefArgs)
