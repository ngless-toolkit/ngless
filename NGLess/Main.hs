{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Main
    ( main
    ) where

import Language
import Validation
import Parse

import System.Console.CmdArgs
import Data.Either
import qualified Data.ByteString as S

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
    input <- readFile fname
    case parse input >> validate of
        Left err -> putStrLn err
        Right expr -> interpret expr
