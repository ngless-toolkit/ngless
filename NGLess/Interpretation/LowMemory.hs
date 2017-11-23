{- Copyright 2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module Interpretation.LowMemory
    ( splitFASTA
    , mergeSamFiles
#ifdef IS_BUILDING_TEST
    , mergeSAMGroups
#endif
    ) where

import qualified Data.ByteString.Char8 as B8
import           Control.Monad.Except

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit as C
import           Data.Conduit ((.|))

import System.IO

import FileManagement
import Output
import NGLess

import Data.Sam
import Data.Fasta
import Utils.Conduit
import Utils.LockFile

splitFASTA :: Int -> FilePath -> FilePath -> NGLessIO [FilePath]
splitFASTA maxBPS ifile ofileBase =
        withLockFile LockParameters
                { lockFname = ifile ++ (show maxBPS) ++ ".split.lock"
                , maxAge = (36 * 3000)
                , whenExistsStrategy = IfLockedRetry { nrLockRetries = 120, timeBetweenRetries = 60 }
                } $ C.runConduit $
            CB.sourceFile ifile
                .| faConduit
                .| splitWriter
    where
        splitWriter = splitWriter' [] (0 :: Int)
        splitWriter' fs n = do
            let f = ofileBase ++ "." ++ show n ++ ".fna"
            getNbps
                .| faWriteC
                .| CB.sinkFileCautious f
            finished <- CC.null
            if finished
                then return (f:fs)
                else splitWriter' (f:fs) (n + 1)
        getNbps = awaitJust $ \fa -> do
                        C.yield fa
                        if faseqLength fa > maxBPS
                            then do
                                lift $ outputListLno' WarningOutput
                                            ["While splitting file '", ifile, ": Sequence ", B8.unpack (seqheader fa), " is ", show (faseqLength fa)
                                            ," bases long (which is longer than the block size). Note that NGLess does not split sequences."]
                                return ()
                            else getNbps' (faseqLength fa)

        getNbps' sofar = awaitJust $ \fa ->
                            if faseqLength fa + sofar > maxBPS
                                then C.leftover fa
                                else do
                                    C.yield fa
                                    getNbps' (faseqLength fa + sofar)


mergeSamFiles :: [FilePath] -> NGLessIO FilePath
mergeSamFiles [] = throwShouldNotOccur "empty input to mergeSamFiles"
mergeSamFiles inputs = do
    outputListLno' TraceOutput ["Merging SAM files: ", show inputs]
    (sam, hout) <- openNGLTempFile (head inputs) "merged_" ".sam"
    C.runConduit $
        -- There are obvious opportunities to make this code take advantage of parallelism
        C.sequenceSources
                    [CB.sourceFile f
                        .| linesC
                        .| readSamGroupsC
                                | f <- inputs]
            .| CL.map mergeSAMGroups
            .| CC.concat
            .| CL.map (ByteLine . encodeSamLine)
            .| byteLineSinkHandle hout
    liftIO $ hClose hout
    return sam

mergeSAMGroups :: [SamGroup] -> SamGroup
mergeSAMGroups groups = group group1 ++ group group2 ++ group groupS
    where
        (group1, group2, groupS) = foldl (\(g1,g2,gS) s ->
                                                (if isFirstInPair s
                                                    then (s:g1, g2, gS)
                                                    else if isSecondInPair s
                                                        then (g1, s:g2, gS)
                                                        else (g1, g2, s:gS))) ([], [], []) $ concat groups
        group :: [SamLine] -> [SamLine]
        group [] = []
        group gs = case filter isAligned gs of
            [] -> [head gs]
            gs' -> gs'

