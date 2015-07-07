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
import Configuration
import ReferenceDatabases
import Output

import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Concurrent
import System.Console.CmdArgs
import System.FilePath.Posix
import System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B


data NGLess =
        DefaultMode
              { debug_mode :: String
              , input :: String
              , script :: Maybe String
              , n_threads :: Int
              , output_directory :: FilePath
              , temporary_directory :: Maybe FilePath
              , keep_temporary_files :: Bool
              }
        | InstallGenMode
              { input :: String}
           deriving (Eq, Show, Data, Typeable)

ngless = DefaultMode
        { debug_mode = "ngless"
        , input = "-" &= argPos 0 &= opt ("-" :: String)
        , script = Nothing &= name "e"
        , n_threads = 1 &= name "n"
        , output_directory = "" &= name "o"
        , temporary_directory = Nothing
        , keep_temporary_files = False
        }
        &= details  [ "Example:" , "ngless script.ngl" ]


installargs = InstallGenMode
        { input = "Reference" &= argPos 0
        }
        &= name "--install-reference-data"
        &= details  [ "Example:" , "(sudo) ngless --install-reference-data sacCer3" ]

-- | function implements the debug-mode argument.
-- The only purpose is to aid in debugging by printing intermediate
-- representations.
function :: String -> String -> Bool -> T.Text -> IO ()
function "ngless" fname reqversion text =
    case parsengless fname reqversion text >>= checktypes >>= validate of
        Left err -> T.putStrLn err
        Right expr -> do
            outputLno' DebugOutput "Validating script..."
            errs <- validate_io expr
            outputLno' InfoOutput "Script OK. Starting interpretation..."
            case errs of
                Nothing -> do
                    interpret fname text (nglBody expr)
                    odir <- outputDirectory
                    writeOutput (odir </> "output.js") fname text
                Just errors -> T.putStrLn (T.concat errors)

function "ast" fname reqversion text = case parsengless fname reqversion text >>= validate of
            Left err -> T.putStrLn (T.concat ["Error in parsing: ", err])
            Right expr -> print . nglBody $ expr

function "tokens" fname _reqversion text = case tokenize fname text of
            Left err -> T.putStrLn err
            Right toks -> print . map snd $ toks

function emode _ _ _ = putStrLn (concat ["Debug mode '", emode, "' not known"])



optsExec opts@DefaultMode{} = do
    let fname = input opts
    setNumCapabilities (n_threads opts)
    setOutputDirectory fname (output_directory opts)
    setTemporaryDirectory (temporary_directory opts)
    setKeepTemporaryFiles (keep_temporary_files opts)
    odir <- outputDirectory
    createDirectoryIfMissing False odir
    --Note that the input for ngless is always UTF-8.
    --Always. This means that we cannot use T.readFile
    --which is locale aware.
    --We also assume that the text file is quite small and, therefore, loading
    --it in to memory is not resource intensive.
    engltext <- case script opts of
        Just s -> return . Right . T.pack $ s
        _ -> T.decodeUtf8' <$> (if fname == "-" then B.getContents else B.readFile fname)
    case engltext of
        Left err -> print err
        Right ngltext -> function (debug_mode opts) fname (isNothing $ script opts) ngltext

-- if user uses the flag -i he will install a Reference Genome to all users
optsExec (InstallGenMode ref)
    | isDefaultReference ref = void $ installData Nothing ref
    | otherwise =
        error (concat ["Reference ", ref, " is not a known reference."])

getModes :: Mode (CmdArgs NGLess)
getModes = cmdArgsMode $ modes [ngless &= auto, installargs]
    &= verbosity
    &= summary sumtext
    &= help "ngless implement the NGLess language"
    where sumtext = concat ["ngless v", versionStr, "(C) NGLess Authors 2013-2015"]

main = cmdArgsRun getModes >>= optsExec
