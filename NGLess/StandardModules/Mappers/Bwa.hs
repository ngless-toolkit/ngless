{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RankNTypes #-}

module StandardModules.Mappers.Bwa
    ( hasValidIndex
    , createIndex
    , callMapper
    ) where

import System.Process
import System.Exit
import System.Directory
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as C
import           GHC.Conc (getNumCapabilities)

import Output
import Configuration
import NGLess

-- | Checks whether all necessary files are present for a BWA index
-- Does not change any file on disk.
hasValidIndex :: FilePath -> NGLessIO Bool
hasValidIndex basepath = doAllFilesExist indexRequiredFormats
    where
        doAllFilesExist [] = return True
        doAllFilesExist (x:xs) = do
            isThere <- liftIO $ doesFileExist (basepath ++ x)
            if isThere
                then doAllFilesExist xs
                else return False
        indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]

-- | Creates bwa index on disk
createIndex :: FilePath -> NGLessIO ()
createIndex fafile = do
    outputListLno' InfoOutput ["Start BWA index creation for ", fafile]
    bwaPath <- bwaBin
    (exitCode, out, err) <- liftIO $
        readProcessWithExitCode bwaPath ["index", fafile] []
    outputListLno' DebugOutput ["BWA-index stderr: ", err]
    outputListLno' DebugOutput ["BWA-index stdout: ", out]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _err -> throwSystemError err

callMapper :: FilePath -> [FilePath] -> [String] -> C.Consumer B.ByteString IO a -> NGLessIO a
callMapper refIndex fps extraArgs outC = do
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    bwaPath <- bwaBin
    numCapabilities <- liftIO getNumCapabilities
    let cmdargs =  concat [["mem", "-t", show numCapabilities, refIndex], extraArgs, fps]
    outputListLno' TraceOutput ["Calling binary ", bwaPath, " with args: ", unwords cmdargs]
    let cp = proc bwaPath cmdargs
    (exitCode, out, err) <- liftIO $
            CP.sourceProcessWithStreams cp
                (return ()) -- stdin
                outC -- stdout
                CL.consume -- stderr
    outputListLno' DebugOutput ["BWA info: ", BL8.unpack $ BL8.fromChunks err]
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done mapping to ", refIndex]
            return out
        ExitFailure code ->
            throwSystemError $ concat ["Failed mapping\nCommand line was::\n\t",
                            bwaPath, unwords cmdargs, "'\n",
                            "Bwa error code was ", show code, "."]

