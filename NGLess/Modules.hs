{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE BangPatterns #-}
module Modules
    ( ArgInformation(..)
    , Function(..)
    , Module(..)
    ) where

import qualified Data.Text as T
import Language
import NGLess

data ArgInformation = ArgInformation
    { argName :: !T.Text
    , argRequired :: !Bool
    , argType :: !NGLType
    , argAllowedSymbols :: !(Maybe [T.Text])
    } deriving (Eq, Show)

data Function = Function
    { funcName :: FuncName
    , funcArgType :: Maybe NGLType
    , funcRetType :: NGLType
    , funcKwArgs :: [ArgInformation]
    , funcAllowsAutoComprehension :: Bool
    } deriving (Eq, Show)

data Module = Module
    { modInfo :: !ModInfo
    , modConstants :: [(T.Text, NGLessObject)]
    , modFunctions :: [Function]
    , runFunction :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
    }

instance Show Module where
    show (Module info cs fs _) = "Module["++show info++"; constants="++show cs++"; functions="++show fs++"]"


