{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Interpret
import Language
import Validation
import Parse

import System.Console.CmdArgs
import Data.Either
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

version = "0.0.0"
data NGLessArgs = NGLessArgs {
        input :: String
    } deriving (Eq, Show, Data, Typeable)

nglessargs = NGLessArgs
        { input = "-" &= argPos 0 &= opt "-"
        } &=
        verbosity &=
        summary sumtext &=
        details ["ngless implement the NGLess language"]
    where sumtext = concat ["ngless v", version, "(C) NGLess Authors 2013"]

main = do
    NGLessArgs fname <- cmdArgs nglessargs
    ngltext <- S.readFile fname
    case parsengless (S8.pack fname) ngltext >>= validate of
        Left err -> S8.putStrLn err
        Right expr -> interpret expr
