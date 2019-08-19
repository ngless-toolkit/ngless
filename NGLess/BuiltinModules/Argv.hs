{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.Argv
    ( loadModule
    ) where

import qualified Data.Text as T
import           Data.Default (def)

import Language

import Modules
import NGLess
import Configuration
import NGLess.NGLEnvironment

loadModule :: T.Text -> NGLessIO Module
loadModule _ = do
    argv <- nConfArgv <$> nglConfiguration
    let nglARGV = NGOList (NGOString <$> argv)
    return def
        { modInfo = ModInfo "builtin.argv" "0.0"
        , modConstants = [("ARGV", nglARGV)]
        }

