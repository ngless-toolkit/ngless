{-# LANGUAGE OverloadedStrings #-}

module ProcessFastQ
    (
    readFastQ,
    readReadSet,
    writeReadSet,
    executeQProc,
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import System.FilePath.Posix
import System.Directory
import Data.Maybe
import Control.Applicative ((<$>))

import FileManagement
import FastQStatistics
import Data.FastQ
import Language
import JSONManager
import Configuration
import Output

writeReadSet :: FilePath -> [ShortRead] -> FastQEncoding -> IO FilePath
writeReadSet fn rs enc = do
    temp <- getTemporaryDirectory 
    newfp <- getTFilePathComp (temp </> (template fn))
    writeGZIP newfp (asFastQ enc rs)
    return newfp


readReadSet :: FastQEncoding -> FilePath -> IO [ShortRead]
readReadSet enc fn = (parseFastQ enc) `fmap` (readPossiblyCompressedFile fn)

executeQProc :: Maybe FastQEncoding -> FilePath -> FilePath -> FilePath -> IO NGLessObject
executeQProc enc f info dirT = do
    let x = dirT ++ "$" ++ info
    readFastQ enc f x dirT

readFastQ :: Maybe FastQEncoding -> FilePath -> FilePath -> FilePath -> IO NGLessObject
readFastQ enc f dst dirT = do
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
        return $ NGOReadSet f enc' (B.pack dirT)
    where
        p s0 s1  = outputList DebugOutput [s0, s1]
        encFromM fd = fromMaybe (guessEncoding . lc $ fd) enc

