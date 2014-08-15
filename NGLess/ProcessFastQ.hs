{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module ProcessFastQ
    (
    parseReadSet,
    readFastQ,
    readReadSet,
    showRead,
    writeReadSet,
    executeQProc
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
    newfp <- getTFilePathComp (temp </> (template $ (B.unpack fn)))
    writeGZIP newfp asFastQ
    return newfp
  where
    asFastQ :: BL.ByteString
    asFastQ = BL.unlines . (fmap (showRead enc)) $ rs 


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
decodeQual enc = B.map (chr . sub . ord)
    where 
        sub v = v - enc
encodeQual enc = B.map (chr . (+) enc . ord)

executeQProc :: FilePath -> FilePath -> FilePath -> IO NGLessObject
executeQProc f info dirT = setupRequiredFiles info dirT >>= \x -> readFastQ f x dirT

readFastQ :: FilePath -> FilePath -> FilePath -> IO NGLessObject
readFastQ f dst dirT = do
        fd <- unCompress f >>= return . computeStats
        p "Generation of statistics for " dst
        createBasicStatsJson (dst ++ "/basicStats.js") fd f -- generate JSON DATA file: basicStats.js
        p "Simple Statistics completed for: " dst
        p "number of base pairs: " (show $ length (qualCounts fd)) 
        p "Lowest char is: " (show $ lc fd)
        printHtmlStatisticsData (qualCounts fd) (enc fd) dst -- " " " file: perBaseQualScoresData.js
        p "Loaded file: " f
        return $ NGOReadSet (B.pack f) (enc fd) (B.pack dirT)
    where
        p s obj = printNglessLn $ s ++ obj
        enc = ord . lc

--remove encodeQual when not Pre-Processing.
showRead :: Int -> NGLessObject -> BL.ByteString
showRead enc (NGOShortRead a b c) = BL.fromChunks [TE.encodeUtf8 a, "\n", b, "\n+\n", (encodeQual enc c)]
showRead _ _ = error "error: The argument must be a read."
