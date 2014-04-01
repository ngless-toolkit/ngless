{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module ProcessFastQ
    (
    parseReadSet,
    readFastQ,
    readReadSet,
    showRead,
    writeReadSet
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Data.Char

import qualified Data.Text.Encoding as TE

import System.FilePath.Posix

import FileManagement
import PerBaseQualityScores
import FastQFileData
import Language
import JSONManager

-- Uncompression of a given fastQ file if it's compressed in a .gz format.



writeReadSet :: B.ByteString -> [NGLessObject] -> Int -> IO FilePath
writeReadSet fn rs enc = do
    temp <- getTemporaryDirectory 
    newfp <- getTFilePathComp (temp </> (snd . splitFileName $ (B.unpack fn)))
    writeGZIP newfp $ asFastQ rs enc
    return newfp

asFastQ :: [NGLessObject] -> Int -> BL.ByteString
asFastQ rs enc = BL.unlines . (fmap (showRead enc)) $ rs 



readReadSet :: Int -> B.ByteString -> IO [NGLessObject]
readReadSet enc fn = (parseReadSet enc) `fmap` (readPossiblyCompressedFile fn)

parseReadSet :: Int -> BL.ByteString -> [NGLessObject]
parseReadSet enc contents = parse' . map BL.toStrict . BL.lines $ contents
        where
            parse' [] = []
            parse' xs = (createRead (Prelude.take 4 xs) : parse' (Prelude.drop 4 xs))
            createRead :: [B.ByteString] -> NGLessObject
            createRead r = case (Prelude.length r) of
                4 -> NGOShortRead (TE.decodeUtf8 $ r !! 0) (r !! 1) (decodeQual enc (r !! 3))
                _ -> error "Number of lines is not multiple of 4!"

-- Change to only apply this function when Pre-Processing
decodeQual enc = B.map (chr . (flip (-) enc) . ord)
encodeQual enc = B.map (chr . (flip (+) enc) . ord)

readFastQ :: FilePath -> FilePath -> FilePath -> IO NGLessObject
readFastQ fname info dirTemplate = do
        contents <- unCompress fname
        let fileData = computeStats contents
        destDir <- setupRequiredFiles info dirTemplate 
        printNglessLn $ "Generation of statistics for " ++ destDir
        createBasicStatsJson (destDir ++ "/basicStats.js") fileData fname -- generate JSON DATA file: basicStats.js
        printNglessLn $ "Simple Statistics for: " ++ destDir ++ " completed "  ++ (show $ length (qualCounts fileData)) ++ " Base pairs."
        printHtmlStatisticsData (qualCounts fileData) (ord (lc fileData)) destDir -- " " " file: perBaseQualScoresData.js
        printNglessLn $ "File: " ++ fname ++ " loaded"
        return $ NGOReadSet (B.pack fname) (ord (lc fileData)) (B.pack dirTemplate)

--remove encodeQual when not Pre-Processing.
showRead :: Int -> NGLessObject -> BL.ByteString
showRead enc (NGOShortRead a b c) = BL.fromChunks [TE.encodeUtf8 a, "\n", b, "\n+\n", (encodeQual enc c)]
showRead _ _ = error "error: The argument must be a read."
