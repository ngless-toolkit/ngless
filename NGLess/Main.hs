{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main
    ( main
    ) where

import FileManagement
import Interpret
import Validation
import Language
import Tokens
import Types
import Parse
import MapInterpretOperation
import WebServer
import Data.DefaultValues

import Control.Applicative
import System.Console.CmdArgs
import System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S


version = "0.0.0"
data NGLessArgs = 
        DefaultMode 
              { debug_mode :: String
              , input :: String
              }
        | InstallGenMode 
              { input :: String}
        | VisualizeMode 
              { port :: Int} 
           deriving (Eq, Show, Data, Typeable)

nglessargs = DefaultMode
        { debug_mode = "ngless"
        , input = "-" &= argPos 0 &= opt ("-" :: String)
        } 
        &= details  [ "Example:" , "ngless -v ../example/script.ngl" ]


installargs = InstallGenMode
        {
            input = "Reference" &= argPos 0
        } 
        &= name "--install-reference-data" 
        &= details  [ "Example:" , "(sudo) ngless -i sacCer3" ]

visualizeargs = VisualizeMode
        {
            port = 8000
        }

-- | function implements the debug-mode argument.
-- The only purpose is to aid in debugging by printing intermediate
-- representations.
function :: String -> String -> T.Text -> IO ()
function "ngless" fname text = case parsengless fname text >>= validate >>= checktypes of
            Left err -> T.putStrLn err
            Right expr -> (interpret text) . nglBody $ expr

function "ast" fname text = case parsengless fname text >>= validate of
            Left err -> T.putStrLn (T.concat ["Error in parsing: ", err])
            Right expr -> putStrLn . show . nglBody $ expr

function "tokens" fname text = case tokenize fname text of
            Left err -> T.putStrLn err
            Right toks -> putStrLn . show . map snd $ toks

function emode _ _ = putStrLn (concat ["Debug mode '", emode, "' not known"])


-- | This function is used to install Genomes globally in /shared directory.
-- Requires SUPER USER permissions
installGenome :: String -> IO ()
installGenome "Reference" = return ()
installGenome ref = do
    hasPerm <- hasPermissions suGenomeDir -- running under SUDO or not.
    _ <- putStrLn $ ref ++ " " ++ (show hasPerm)
    case hasPerm of
        True -> configGenome ref Root >> return ()
        False -> configGenome ref User >> return ()


optsExec (DefaultMode dmode fname) = do
    --Note that the input for ngless is always UTF-8.
    --Always. This means that we cannot use T.readFile
    --which is locale aware.
    --We also assume that the text file is quite small and, therefore, loading
    --it in to memory is not resource intensive.
    getCurrentDirectory >>= print
    _ <- defaultDir >>= createDirIfNotExists  -- this is the dir where everything will be kept.
    engltext <- T.decodeUtf8' <$> (if fname == "-" then S.getContents else S.readFile fname)
    case engltext of
        Left err -> putStrLn (show err)
        Right ngltext -> function dmode fname ngltext

-- if user uses the flag -i he will install a Reference Genome to all users
optsExec (InstallGenMode ref) = installGenome ref
optsExec (VisualizeMode p) = runWebServer p

getModes :: Mode (CmdArgs NGLessArgs)
getModes = cmdArgsMode $ modes [nglessargs &= auto, installargs, visualizeargs]
    &= verbosity
    &= summary sumtext  
    &= help "ngless implement the NGLess language"
    where sumtext = concat ["ngless v", version, "(C) NGLess Authors 2013"]

main = do
    args' <- cmdArgsRun getModes
    optsExec args'
    
