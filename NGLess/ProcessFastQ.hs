{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module ProcessFastQ
    (
    readReadSet,
    writeReadSet,
    executeQProc,
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Compression.GZip as GZip

import System.IO
import System.Directory
import Data.Maybe
import Control.Applicative ((<$>))

import FileManagement
import FastQStatistics
import Data.FastQ
import Language
import JSONManager
import Output

writeGZIP :: Handle -> BL.ByteString -> IO ()
writeGZIP h = BL.hPut h . GZip.compress

writeReadSet :: FilePath -> [ShortRead] -> FastQEncoding -> IO FilePath
writeReadSet fn rs enc = do
    (newfp,h) <- openNGLTempFile fn "" "fq.gz"
    writeGZIP h (asFastQ enc rs)
    hClose h
    return newfp


readReadSet :: FastQEncoding -> FilePath -> IO [ShortRead]
readReadSet enc fn = (parseFastQ enc) `fmap` (readPossiblyCompressedFile fn)

-- ^ Process quality.
executeQProc :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> FilePath         -- ^ destination for statistics
                -> IO NGLessObject
executeQProc enc f dst = do
        fd <- computeStats <$> readPossiblyCompressedFile f
        let enc' = encFromM fd -- when Nothing calculate encoding, else use value from Just.
        p "Generation of statistics for " dst
        let json = createBasicStatsJson  fd f enc' -- generate JSON DATA file: basicStats.js
        createDirectoryIfMissing True dst
        BL.writeFile (dst ++ "/basicStats.js") json
        p "Simple Statistics completed for: " dst
        p "Number of base pairs: "      (show $ length (qualCounts fd)) 
        p "Encoding is: "               (show enc')
        p "Number of sequences: "   (show $ nSeq fd)
        printHtmlStatisticsData (qualCounts fd) enc' dst -- " " " file: perBaseQualScoresData.js
        p "Loaded file: " f
        return $ NGOReadSet1 enc' f
    where
        p s0 s1  = outputListLno' DebugOutput [s0, s1]
        encFromM fd = fromMaybe (guessEncoding . lc $ fd) enc

