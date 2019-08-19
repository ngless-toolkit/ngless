{- Copyright 2016-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module BuiltinModules.Readlines
    ( loadModule
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))
import           Data.Default (def)

import Language

import Modules
import NGLess
import Utils.Conduit (ByteLine(..), linesC)

executeReadlines :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReadlines (NGOString fname) _ = do
    content <-
        C.runConduit $ (CC.sourceFile (T.unpack fname) `C.catchC` (\(e :: IOError) ->
                                            throwDataError ("Could not read file '"++T.unpack fname++"': " ++ show e)))
            .| linesC
            .| CL.consume
    return $! NGOList [NGOString (T.pack . B8.unpack $ ell) | ByteLine ell <- content]
executeReadlines arg _ = throwShouldNotOccur ("executeReadlines called with argument: " ++ show arg)

readlines_Function = Function
    { funcName = FuncName "readlines"
    , funcArgType = Just NGLString
    , funcArgChecks = [ArgCheckFileReadable]
    , funcRetType = NGList NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = [FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.readlines" "0.0"
    , modFunctions = [readlines_Function]
    , runFunction = const executeReadlines
    }

