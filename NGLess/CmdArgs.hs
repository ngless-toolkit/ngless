{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
module CmdArgs
    ( ColorSetting(..)
    , NGLessModes(..)
    , nglessModes
    ) where

{-| This is a separate module so that Main & Configuration
 - can share these objects. Putting them in Configuration would, however,
 - pollute it as it is imported from everywhere.
 -}

import System.Console.CmdArgs
import qualified Data.Configurator.Types as CF

data ColorSetting = AutoColor | NoColor | ForceColor
    deriving (Eq, Data, Typeable, Show)

instance CF.Configured ColorSetting where
    convert (CF.String "auto") = Just AutoColor
    convert (CF.String "force") = Just ForceColor
    convert (CF.String "none") = Just NoColor
    convert _ = Nothing

data NGLessModes =
        DefaultMode
              { debug_mode :: String
              , input :: String
              , script :: Maybe String
              , print_last :: Bool
              , trace_flag :: Maybe Bool
              , nThreads :: Int
              , createOutputDirectory :: Bool
              , output_directory :: Maybe FilePath
              , temporary_directory :: Maybe FilePath
              , keep_temporary_files :: Maybe Bool
              , config_files :: Maybe [FilePath]
              , color :: Maybe ColorSetting
              , no_header :: Bool
              , subsampleMode :: Bool
              , extraArgs :: [String]
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


color_setting :: Annotate Ann
color_setting =
        enum_ color [atom (Nothing :: Maybe ColorSetting) += help "auto color"
            ,atom (Just AutoColor) += help "auto color" += name "auto"
            ,atom (Just NoColor) += help "no color" += name "no-color"
            ,atom (Just ForceColor) += help "force color (even if output is not a terminal)" += name "color"
            ]

nglessArgs = record DefaultMode{}
        [ debug_mode := "ngless"
        , input := "-" += argPos 0 += opt ("-" :: String)
        , script := Nothing += name "e"
        , trace_flag := Nothing += name "trace"
        , print_last := False += name "p"
        , nThreads := 1 += name "n" += name "j"
        , createOutputDirectory := True += name "create-output-directory" += help "create an ngless_output directory"
        , output_directory := Nothing += name "o"
        , temporary_directory := Nothing += name "t"
        , keep_temporary_files := Nothing
        , config_files := Nothing += name "conf"
        , color_setting
        , no_header := False += name "no-header" += help "Do not print version header"
        , subsampleMode := False += name "subsample" += help "Turn on sub-sampling"
        , extraArgs := [] += args
        ]
        += details ["Example:" , "ngless script.ngl"]

installArgs = record InstallGenMode{}
        [ input := "Reference" += argPos 0
        , color_setting
        ]
        += name "--install-reference-data"
        += details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

createRefArgs = record CreateReferencePackMode{}
        [ oname := "" += argPos 0
        , genome_url := "" += name "g"
        , gtf_url := "" += name "a"
        , color_setting
        ]
        += name "--create-reference-pack"
        += details ["Example:", "ngless --create-reference-pack ref.tar.gz -g http://...genome.fa.gz -a http://...gtf.fa.gz"]


nglessModes = modes_ [nglessArgs += auto, installArgs, createRefArgs]
