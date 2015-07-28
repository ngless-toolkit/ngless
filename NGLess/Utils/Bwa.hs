{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

module Utils.Bwa
    ( hasValidIndex
    , createIndex
    ) where

import System.Process
import System.Exit
import System.Directory
import Control.Monad.IO.Class (liftIO)

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
        ExitFailure _err -> error err

