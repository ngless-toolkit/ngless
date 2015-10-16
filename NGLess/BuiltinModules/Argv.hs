{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.Argv
    ( loadModule
    ) where

import qualified Data.Text as T
import Control.Applicative ((<$>))

import Language
import Configuration

import Modules
import NGLess

loadModule :: T.Text -> NGLessIO Module
loadModule _ = do
    argv <- nConfArgv <$> nglConfiguration
    let nglARGV = NGOList (NGOString <$> argv)
    return $ Module
        { modInfo = ModInfo "builtin.argv" "0.0"
        , modConstants = [("ARGV", nglARGV)]
        , modFunctions = []
        , runFunction = \_ _ _ -> return NGOVoid
        , validateFunction = const (return [])
        }

