{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.Argv
    ( loadModule
    ) where

import qualified Data.Text as T
import Data.Default

import Language
import Configuration

import Modules
import NGLess

loadModule :: T.Text -> NGLessIO Module
loadModule _ = do
    argv <- nConfArgv <$> nglConfiguration
    let nglARGV = NGOList (NGOString <$> argv)
    return def
        { modInfo = ModInfo "builtin.argv" "0.0"
        , modConstants = [("ARGV", nglARGV)]
        }

