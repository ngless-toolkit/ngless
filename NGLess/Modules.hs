{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE BangPatterns #-}
module Modules
    ( ArgInformation(..)
    , Function(..)
    , Module(..)
    , ModInfo(..) -- re-export
    ) where

import qualified Data.Text as T
import Language
import NGLess

-- | Basic information about argument to a function
data ArgInformation = ArgInformation
    { argName :: !T.Text -- ^ argument name
    , argRequired :: !Bool -- ^ whether it is required
    , argType :: !NGLType -- ^  type
    , argAllowedSymbols :: !(Maybe [T.Text]) -- ^ if type is symbol, what are the allowed symbolx
    } deriving (Eq, Show)

data Function = Function
    { funcName :: FuncName -- ^ name of function
    , funcArgType :: Maybe NGLType -- ^ if it takes an unnamed argument, what is its type
    , funcRetType :: NGLType -- ^ what type it returns
    , funcKwArgs :: [ArgInformation] -- ^ what are the keyword arguments
    , funcAllowsAutoComprehension :: Bool -- ^ if true, then calling this function with [funcArgType] should return [funcRetType]
    } deriving (Eq, Show)

data Module = Module
    { modInfo :: !ModInfo -- ^ name & version
    , modConstants :: [(T.Text, NGLessObject)] -- ^ constants defined in this module
    , modFunctions :: [Function] -- ^ functions defined by this module
    , runFunction :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject -- ^ run a function defined by the module. The first argument is the name of the function
    , validateFunction :: [(Int,Expression)] -> NGLessIO [T.Text] -- ^ this is called before any interpretation, should return a list of error messages (empty if no errors)
    }

instance Show Module where
    show (Module info cs fs _ _) = "Module["++show info++"; constants="++show cs++"; functions="++show fs++"]"


