{- Copyright 2016-2017 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.Readlines
    ( loadModule
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit ((=$=), ($$))
import Data.Default

import Language

import Modules
import NGLess

executeReadlines :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReadlines (NGOString fname) _ = do
    content <- C.sourceFile (T.unpack fname)
            =$= CB.lines
            $$ CL.consume
    return $! NGOList [NGOString (T.pack . B8.unpack $ ell) | ell <- content]
executeReadlines arg _ = throwShouldNotOccur ("executeReadlines called with argument: " ++ show arg)

readlines_Function = Function
    { funcName = FuncName "readlines"
    , funcArgType = Just NGLString
    , funcArgChecks = [ArgCheckFileReadable]
    , funcRetType = NGList NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.readlines" "0.0"
    , modFunctions = [readlines_Function]
    , runFunction = const executeReadlines
    }

