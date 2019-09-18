{- Copyright 2013-2019 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RankNTypes #-}

module StandardModules.Mappers.Bwa
    ( hasValidIndex
    , createIndex
    , callMapper
    ) where

import           System.Directory (doesFileExist)
import           System.Posix (getFileStatus, fileSize, FileOffset)
import           System.Path (splitExt)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B

import qualified Data.Conduit as C
import           Control.Monad.Extra (allM)
import           Control.Concurrent (getNumCapabilities)

import Output
import NGLess
import Data.FastQ
import Configuration
import NGLess.NGLEnvironment
import Dependencies.Versions (bwaVersion)
import FileManagement (bwaBin)
import Utils.Process (runProcess)

-- | Appends bwa version to the index such that different versions
-- of bwa use different indices
indexPrefix :: FilePath -> NGLessIO FilePath
indexPrefix base = do
    let (basename, ext) = splitExt base
    return $ basename ++ "-bwa-" ++ bwaVersion ++ ext

-- | Checks whether all necessary files are present for a BWA index
-- Does not change any file on disk.
hasValidIndex :: FilePath -> NGLessIO Bool
hasValidIndex basepath = do
    base <- indexPrefix basepath
    let indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]
    liftIO $ allM (doesFileExist . (base ++)) indexRequiredFormats

-- BWA's default indexing parameters are quite conservative. This leads to
-- a small memory footprint at the cost of more CPU hours.
-- With large databases (~100GB) default settings require over 2 weeks of
-- CPU time. Increasing the default blocksize will increase the memory
-- footprint but will reduce indexing time 3 to 6 fold.
--
-- This patch increases the blocksize to roughly 1/10th of the filesize.
-- The memory footprint should be about the size of the database.
--
-- As per https://github.com/lh3/bwa/issues/104 this patch may become
-- obsolete once this functionality is built into bwa.
--
-- | Checks whether we should customize bwa's indexing blocksize
customBlockSize :: FilePath -> IO [String]
customBlockSize path = sizeAsParam . fileSize <$> getFileStatus path

sizeAsParam :: FileOffset -> [String]
sizeAsParam size
    | size >= minimalsize = ["-b", show $ div size factor]
    | otherwise = []
        where minimalsize = 100*1000*1000 -- 100MB - if smaller, use software's default
              factor = 10

-- | Creates bwa index on disk
createIndex :: FilePath -> NGLessIO ()
createIndex fafile = do
    outputListLno' InfoOutput ["Start BWA index creation for ", fafile]
    blocksize <- liftIO $ customBlockSize fafile
    prefix <- indexPrefix fafile
    bwaPath <- bwaBin
    runProcess
            bwaPath
            (["index"] ++ blocksize ++ ["-p", prefix, fafile])
            (return ())
            (Left ())

callMapper :: FilePath -> ReadSet -> [String] -> C.ConduitT B.ByteString C.Void NGLessIO a -> NGLessIO a
callMapper refIndex rs extraArgs outC = do
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    bwaPath <- bwaBin
    refIndex' <- indexPrefix refIndex
    numCapabilities <- liftIO getNumCapabilities
    strictThreads <- nConfStrictThreads <$> nglConfiguration
    let bwathreads
            | strictThreads && numCapabilities > 1 = numCapabilities - 1
            | otherwise = numCapabilities
                                                    -- -K 100000000 is a hidden option to set the chunk size
                                                    -- this makes the output independent of the number of threads
        cmdargs =  concat [["mem", "-t", show bwathreads, "-K", "100000000"], extraArgs, [refIndex', "-p", "-"]]
    runProcess
            bwaPath
            cmdargs
            (interleaveFQs rs) -- stdin
            (Right outC) -- stdout
