{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
module Main
    ( main
    ) where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent
import System.Console.CmdArgs
import System.FilePath.Posix
import System.Directory
import System.IO (stderr, hPutStrLn)
import System.Console.ANSI
import System.Exit (exitSuccess, exitFailure)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Interpret
import Validation
import ValidationNotPure
import Language
import Types
import Parse
import Configuration
import ReferenceDatabases
import Output
import NGLess
import Modules
import CmdArgs
import StandardModules.NGLStdlib

import qualified BuiltinModules.AsReads as ModAsReads

-- | wrapPrint transforms the script by transforming the last expression <expr>
-- into write(<expr>, ofile=STDOUT)
wrapPrint (Script v sc) = wrap sc >>= Right . Script v
    where
        wrap [] = Right []
        wrap [(lno,e)]
            | wrapable e = Right [(lno,addPrint e)]
            | otherwise = Left "Cannot add write() statement at the end of script (the script cannot terminate with a print/write call)"
        wrap (e:es) = wrap es >>= Right . (e:)
        addPrint e = FunctionCall (FuncName "write") e [(Variable "ofile", BuiltinConstant (Variable "STDOUT"))] Nothing

        wrapable (FunctionCall (FuncName f) _ _ _)
            | f `elem` ["print", "write"] = False
        wrapable _ = True

rightOrDie :: Either T.Text a -> IO a
rightOrDie (Left err) = fatalError (T.unpack err)
rightOrDie (Right v) = return v

fatalError :: String -> IO b
fatalError err = do
    let st = setSGRCode [SetColor Foreground Dull Red]
    hPutStrLn stderr (st ++ err)
    hPutStrLn stderr "Exiting after fatal error..."
    exitFailure

whenStrictlyNormal act = do
    v <- getVerbosity
    when (v == Normal) act

runNGLessIO :: String -> NGLessIO a -> IO a
runNGLessIO context act = runResourceT (runExceptT act) >>= \case
        Left m -> do
            putStrLn ("Error occurred: "++context)
            print m
            exitFailure
        Right v -> return v

loadModules :: [ModInfo] -> NGLessIO [Module]
loadModules mods  = do
    mA <- ModAsReads.loadModule ("" :: T.Text)
    imported <- loadStdlibModules mods
    return (mA:imported)

printHeader :: IO ()
printHeader = putStr
    ("NGLess v"++versionStr++" (C) NGLess authors\n"++
    "\n"++
    "http://luispedro.github.io/ngless\n"++
    "\n")

optsExec :: NGLess -> IO ()
optsExec opts@DefaultMode{} = do
    let fname = input opts
    let reqversion = isNothing $ script opts
    setNumCapabilities (nThreads opts)
    initConfiguration opts
    engltext <- case script opts of
        Just s -> return . Right . T.pack $ s
        _ -> T.decodeUtf8' <$> (if fname == "-" then B.getContents else B.readFile fname)
    ngltext <- rightOrDie (case engltext of { Right e -> Right e ; Left b -> Left . T.pack . show $ b })
    let maybe_add_print = (if print_last opts then wrapPrint else Right)
    let parsed = parsengless fname reqversion ngltext >>= maybe_add_print
    sc' <- rightOrDie parsed
    when (debug_mode opts == "ast") $ do
        forM_ (nglBody sc') $ \(lno,e) ->
            putStrLn ((if lno < 10 then " " else "")++show lno++": "++show e)
        exitSuccess
    modules <- runNGLessIO "loading modules" $ loadModules (fromMaybe [] (nglHeader sc' >>= Just . nglModules))
    sc <- rightOrDie $ checktypes modules sc' >>= validate modules
    when (uses_STDOUT `any` [e | (_,e) <- nglBody sc]) $
        whenStrictlyNormal (setVerbosity Quiet)
    odir <- runNGLessIO "cannot fail" outputDirectory
    createDirectoryIfMissing False odir
    unless (no_header opts) printHeader
    runNGLessIO "running script" $ do
        --Note that the input for ngless is always UTF-8.
        --Always. This means that we cannot use T.readFile
        --which is locale aware.
        --We also assume that the text file is quite small and, therefore, loading
        --it in to memory is not resource intensive.
        outputLno' DebugOutput "Validating script..."
        errs <- liftIO (validate_io sc)
        when (isJust errs) $
            liftIO (rightOrDie (Left . T.concat . map (T.pack . show) . fromJust $ errs))
        outputLno' InfoOutput "Script OK. Starting interpretation..."
        interpret modules (nglBody sc)
    writeOutput (odir </> "output.js") fname ngltext
    exitSuccess


-- if user uses the flag -i he will install a Reference Genome to all users
optsExec (InstallGenMode ref _)
    | isDefaultReference ref = void . runNGLessIO "installing data" $ installData Nothing ref
    | otherwise =
        error (concat ["Reference ", ref, " is not a known reference."])

optsExec (CreateReferencePackMode ofile gen gtf _) = runNGLessIO "creating reference package" $ do
        outputLno' InfoOutput "Starting packaging (will download and index genomes)..."
        createReferencePack ofile gen gtf

getModes :: Mode (CmdArgs NGLess)
getModes = cmdArgsMode $ modes [nglessArgs &= auto, installArgs, createRefArgs]
    &= verbosity
    &= summary sumtext
    &= help "ngless implement the NGLess language"
    &= helpArg [name "h"]
    where sumtext = concat ["ngless v", versionStr, "(C) NGLess Authors 2013-2015"]

main = cmdArgsRun getModes >>= optsExec
