{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Example
    ( loadModule
    ) where

import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Language
import Modules
import NGLess

execute :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
execute fname input args = liftIO $do
    putStrLn ("Called example function '"++T.unpack fname++"'")
    putStrLn ("First argument is "++show input)
    putStrLn ("Keyword arguments are "++show args)
    putStrLn "This function does nothing except print this information"
    return input

exampleFunction = Function
    { funcName = FuncName "example"
    , funcArgType = Just NGLReadSet
    , funcRetType = NGLReadSet
    , funcKwArgs =
            [ArgInformation "opt1" False NGLSymbol (Just ["test1", "test2"])
            ]
    , funcAllowsAutoComprehension = True
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return Module
    { modInfo = ModInfo "stdlib.example" "0.0"
    , modConstants =
            [("EXAMPLE_0", NGOInteger 0)
            ,("EXAMPLE_TRUE", NGOBool True)
            ,("EXAMPLE_HELLO", NGOString "Hello")
            ]
    , modFunctions = [exampleFunction]
    , runFunction = execute
    , validateFunction = const (return [])
    }

