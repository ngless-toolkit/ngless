{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module ProcessFastQ
    (
    parseReadSet,
    readFastQ,
    readReadSet,
    showRead,
    writeReadSet,
    executeQProc,
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import System.FilePath.Posix

import FileManagement
import FastQStatistics
import Data.FastQ
import Language
import JSONManager

-- Uncompression of a given fastQ file if it's compressed in a .gz format.

writeReadSet :: B.ByteString -> [ShortRead] -> FastQEncoding -> IO FilePath
writeReadSet fn rs enc = do
    temp <- getTemporaryDirectory 
    newfp <- getTFilePathComp (temp </> (template $ (B.unpack fn)))
    writeGZIP newfp asFastQ
    return newfp
  where
    asFastQ :: BL.ByteString
    asFastQ = BL.unlines . (fmap (showRead enc)) $ rs 


readReadSet :: FastQEncoding -> B.ByteString -> IO [ShortRead]
readReadSet enc fn = (parseReadSet enc) `fmap` (readPossiblyCompressedFile fn)

executeQProc :: Maybe FastQEncoding -> FilePath -> FilePath -> FilePath -> IO NGLessObject
executeQProc enc f info dirT = setupRequiredFiles info dirT >>= \x -> readFastQ enc f x dirT

readFastQ :: Maybe FastQEncoding -> FilePath -> FilePath -> FilePath -> IO NGLessObject
readFastQ enc f dst dirT = do
        fd <- unCompress f >>= return . computeStats
        let enc' = encFromM fd -- when Nothing calculate encoding, else use value from Just.
        p "Generation of statistics for " dst
        let json = createBasicStatsJson  fd f enc' -- generate JSON DATA file: basicStats.js
        BL.writeFile (dst ++ "/basicStats.js") json
        p "Simple Statistics completed for: " dst
        p "Number of base pairs: "      (show $ length (qualCounts fd)) 
        p "Encoding is: "               (show $ enc')
        p "Number of sequences: "   (show $ nSeq fd)
        printHtmlStatisticsData (qualCounts fd) enc' dst -- " " " file: perBaseQualScoresData.js
        p "Loaded file: " f
        return $ NGOReadSet (B.pack f) enc' (B.pack dirT)
    where
        p s obj  = printNglessLn $ s ++ obj
        encFromM fd = maybe (guessEncoding . lc $ fd) id enc

