{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Interpret
import Validation
import Tokens
import Parse

import Control.Applicative
import System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S

version = "0.0.0"
data NGLessArgs = NGLessArgs
    { debug_mode :: String
    , input :: String
    } deriving (Eq, Show, Data, Typeable)

nglessargs = NGLessArgs
        { debug_mode = "ngless"
        , input = "-" &= argPos 0 &= opt "-"
        } &=
        verbosity &=
        summary sumtext &=
        details ["ngless implement the NGLess language"]
    where sumtext = concat ["ngless v", version, "(C) NGLess Authors 2013"]


-- | function implements the debug-mode argument.
-- The only purpose is to aid in debugging by printing intermediate
-- representations.
function :: String -> String -> T.Text -> IO ()
function "ngless" fname text = case parsengless fname text >>= validate of
            Left err -> T.putStrLn err
            Right expr -> interpret expr

function "tokens" fname text = case tokenize fname text of
            Left err -> T.putStrLn err
            Right toks -> putStrLn $ show toks

function emode _ _ = putStrLn (concat ["Debug mode '", emode, "' not known"])

main = do
    NGLessArgs dmode fname <- cmdArgs nglessargs
    --Note that the input for ngless is always UTF-8.
    --Always. This means that we cannot use T.readFile
    --which is locale aware
    engltext <- T.decodeUtf8' <$> (if fname == "-" then S.getContents else S.readFile fname)
    case engltext of
        Left err -> putStrLn (show err)
        Right ngltext -> function dmode fname ngltext
