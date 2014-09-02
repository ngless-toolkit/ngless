module Utils.Bwa
    ( hasValidIndex
    , createIndex
    ) where

import System.Process
import System.Exit
import System.Directory

import Configuration

-- | Checks whether all necessary files are present for a BWA index
-- Does not change any file on disk.
hasValidIndex :: FilePath -> IO Bool
hasValidIndex basepath = doAllFilesExist indexRequiredFormats
    where
        doAllFilesExist [] = return True
        doAllFilesExist (x:xs) = do
            isThere <- doesFileExist (basepath ++ x)
            if isThere
                then doAllFilesExist xs
                else return False
        indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]

-- | Creates bwa index on disk
createIndex :: FilePath -> IO ()
createIndex fafile = do
    bwaPath <- bwaBin
    (exitCode, out, err) <-
        readProcessWithExitCode bwaPath ["index", fafile] []
    printNglessLn err
    printNglessLn out
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _err -> error err
