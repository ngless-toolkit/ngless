{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Interpret
import Validation
import Parse

import Control.Applicative
import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
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
    --Note that the input for ngless is always UTF-8.
    --Always. This means that we cannot use T.readFile
    --which is locale aware
    engltext <- T.decodeUtf8' <$> (if fname == "-" then S.getContents else S.readFile fname)
    case engltext of
        Left err -> putStrLn (show err)
        Right ngltext -> case parsengless fname ngltext >>= validate of
            Left err -> T.putStrLn err
            Right expr -> interpret expr
