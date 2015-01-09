{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main
    ( main
    ) where

import Interpret
import Validation
import ValidationNotPure
import Language
import Tokens
import Types
import Parse
import WebServer
import Configuration
import ReferenceDatabases
import Output

import Control.Monad
import Control.Applicative
import Control.Concurrent
import System.Console.CmdArgs
import System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S


data NGLess =
        DefaultMode
              { debug_mode :: String
              , input :: String
              , n_threads :: Int
              }
        | InstallGenMode
              { input :: String}
        | VisualizeMode
              { input :: FilePath
              , port :: Int
              }
           deriving (Eq, Show, Data, Typeable)

ngless = DefaultMode
        { debug_mode = "ngless"
        , input = "-" &= argPos 0 &= opt ("-" :: String)
        , n_threads = 1 &= name "n"
        }
        &= details  [ "Example:" , "ngless script.ngl" ]


installargs = InstallGenMode
        {
            input = "Reference" &= argPos 0
        }
        &= name "--install-reference-data"
        &= details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

visualizeargs = VisualizeMode
        { port = 8000
        , input = "-" &= argPos 0
        } &= name "--visualize"

-- | function implements the debug-mode argument.
-- The only purpose is to aid in debugging by printing intermediate
-- representations.
function :: String -> String -> T.Text -> IO ()
function "ngless" fname text =
    case parsengless fname text >>= checktypes >>= validate of
        Left err -> T.putStrLn err
        Right expr -> do
            outputLno' DebugOutput "Validating script..."
            errs <- validate_io expr
            outputLno' InfoOutput "Script OK. Starting interpretation..."
            case errs of
                Nothing -> interpret fname text (nglBody expr)
                Just errors -> T.putStrLn (T.concat errors)

function "ast" fname text = case parsengless fname text >>= validate of
            Left err -> T.putStrLn (T.concat ["Error in parsing: ", err])
            Right expr -> print . nglBody $ expr

function "tokens" fname text = case tokenize fname text of
            Left err -> T.putStrLn err
            Right toks -> print . map snd $ toks

function emode _ _ = putStrLn (concat ["Debug mode '", emode, "' not known"])



optsExec (DefaultMode dmode fname n) = do
    setNumCapabilities n
    odir <- outputDirectory fname
    createDirectoryIfMissing False odir
    --Note that the input for ngless is always UTF-8.
    --Always. This means that we cannot use T.readFile
    --which is locale aware.
    --We also assume that the text file is quite small and, therefore, loading
    --it in to memory is not resource intensive.
    engltext <- T.decodeUtf8' <$> (if fname == "-" then S.getContents else S.readFile fname)
    case engltext of
        Left err -> print err
        Right ngltext -> function dmode fname ngltext

-- if user uses the flag -i he will install a Reference Genome to all users
optsExec (InstallGenMode ref)
    | isDefaultReference ref = void $ installData Nothing ref
    | otherwise =
        error (concat ["Reference ", ref, " is not a known reference."])
optsExec (VisualizeMode fname p) = runWebServer fname p

getModes :: Mode (CmdArgs NGLess)
getModes = cmdArgsMode $ modes [ngless &= auto, installargs, visualizeargs]
    &= verbosity
    &= summary sumtext
    &= help "ngless implement the NGLess language"
    where sumtext = concat ["ngless v", versionStr, "(C) NGLess Authors 2013-2015"]

main = cmdArgsRun getModes >>= optsExec
