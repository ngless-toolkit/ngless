{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
module CmdArgs
    ( ColorSetting(..)
    , NGLess(..)
    , nglessArgs
    , installArgs
    , createRefArgs
    ) where

{-| This is a separate module so that Main & Configuration
 - can share these objects. Putting them in Configuration would, however,
 - pollute it as it is imported from everywhere.
 -}

import System.Console.CmdArgs

data ColorSetting = AutoColor | NoColor | ForceColor deriving (Eq, Data, Typeable, Show)
data NGLess =
        DefaultMode
              { debug_mode :: String
              , input :: String
              , script :: Maybe String
              , print_last :: Bool
              , trace_flag :: Maybe Bool
              , nThreads :: Int
              , output_directory :: Maybe FilePath
              , temporary_directory :: Maybe FilePath
              , keep_temporary_files :: Maybe Bool
              , config_files :: Maybe [FilePath]
              , color :: Maybe ColorSetting
              , no_header :: Bool
              }
        | InstallGenMode
              { input :: String
              , color :: Maybe ColorSetting
              }
        | CreateReferencePackMode
              { oname :: FilePath
              , genome_url :: String
              , gtf_url :: String
              , color :: Maybe ColorSetting
              }
           deriving (Eq, Show, Data, Typeable)

nglessArgs = DefaultMode
        { debug_mode = "ngless"
        , input = "-" &= argPos 0 &= opt ("-" :: String)
        , script = Nothing &= name "e"
        , trace_flag = Nothing &= name "trace"
        , print_last = False &= name "p"
        , nThreads = 1 &= name "n"
        , output_directory = Nothing &= name "o"
        , temporary_directory = Nothing &= name "t"
        , keep_temporary_files = Nothing
        , config_files = Nothing &= name "conf"
        , color = Just $ enum [AutoColor &= help "auto color", NoColor &= help "no color" &= name "no-color", ForceColor &= name "color"]
        , no_header = False &= name "no-header" &= help "Do not print version header"
        }
        &= details  [ "Example:" , "ngless script.ngl" ]

installArgs = InstallGenMode
        { input = "Reference" &= argPos 0
        , color = Just $ enum [AutoColor &= help "auto color", NoColor &= help "no color" &= name "no-color", ForceColor &= name "color"]
        }
        &= name "--install-reference-data"
        &= details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

createRefArgs = CreateReferencePackMode
        { oname = "" &= argPos 0
        , genome_url = "" &= name "g"
        , gtf_url = "" &= name "a"
        , color = Just $ enum [AutoColor &= help "auto color", NoColor &= help "no color" &= name "no-color", ForceColor &= name "color"]
        } &= name "--create-reference-pack"
        &= details ["Example:", "ngless --create-reference-pack ref.tar.gz -g http://...genome.fa.gz -a http://...gtf.fa.gz"]


