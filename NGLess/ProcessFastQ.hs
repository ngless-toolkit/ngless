{-# LANGUAGE BangPatterns #-}

module ProcessFastQ
    (
    parseReadSet,
    readFastQ,
    readPossiblyCompressedFile,
    readReadSet,
    writeToFile,
    showRead,
    unCompress,
    writeGZIP,
    writeReadSet,
    appendFile'
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Codec.Compression.GZip as GZip    
import qualified Data.Map as Map

import Data.Char

import System.FilePath.Posix

import FileManagement
import PerBaseQualityScores
import PrintFastqBasicStats
import FastQFileData
import Language
    

-- Uncompression of a given fastQ file if it's compressed in a .gz format.
unCompress fname =
    if T.isInfixOf (T.pack ".gz") (T.pack fname)
        then fmap GZip.decompress (BL.readFile fname)
        else BL.readFile fname -- not compressed

appendFile' = BL.appendFile

getNGOString (Just (NGOString s)) = T.unpack s
getNGOString _ = "Error: Type is different of String"

writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO (B.ByteString, NGLessObject)   
writeToFile (NGOReadSet path enc) args = do
    let map' = Map.fromList args
        destFilePath = Map.lookup (T.pack "ofile") map'
        newfp = getNGOString destFilePath
    copyFile (B.unpack path) newfp
    return $ (path, NGOReadSet (B.pack newfp) enc)

writeToFile _ _ = error "Error: writeToFile Not implemented yet"

writeReadSet :: B.ByteString -> [NGLessObject] -> Int -> IO FilePath
writeReadSet fn rs enc = do
    temp <- getTemporaryDirectory 
    newfp <- getTFilePathComp (temp </> (snd . splitFileName $ (B.unpack fn)))
    writeGZIP newfp $ asFastQ rs enc
    return newfp

asFastQ :: [NGLessObject] -> Int -> BL.ByteString
asFastQ rs enc = BL.unlines . (fmap (showRead enc)) $ rs 

writeGZIP :: String -> BL.ByteString -> IO ()
writeGZIP fp contents = BL.writeFile fp $ GZip.compress contents 

readPossiblyCompressedFile ::  B.ByteString -> IO BL.ByteString
readPossiblyCompressedFile fileName = unCompress (B.unpack fileName)

readReadSet :: Int -> B.ByteString -> IO [NGLessObject]
readReadSet enc fn = (parseReadSet enc) `fmap` (readPossiblyCompressedFile fn)

parseReadSet :: Int -> BL.ByteString -> [NGLessObject]
parseReadSet enc contents = parse' . (fmap BL.unpack) . BL.lines $ contents
        where
            parse' [] = []
            parse' xs = (createRead (Prelude.take 4 xs) : parse' (Prelude.drop 4 xs))
            createRead :: [String] -> NGLessObject
            createRead r = case (Prelude.length r) of
                4 -> NGOShortRead (T.pack $ r !! 0) (B.pack $ r !! 1) (B.pack $ decodeQual enc (r !! 3))
                _ -> error "Number of lines is not multiple of 4!"

decodeQual enc = fmap (chr . (flip (-) enc) . ord)
encodeQual enc = fmap (chr . (flip (+) enc) . ord)

readFastQ :: FilePath -> IO NGLessObject
readFastQ fname = do
        contents <- unCompress fname
        let fileData = iterateFile contents
        destDir <- setupRequiredFiles fname
        putStrLn $ "Generation of statistics for " ++ destDir
        createBasicStatsJson (destDir ++ "/basicStats.js") fileData fname -- generate JSON DATA file: basicStats.js
        putStrLn $ "Simple Statistics for: " ++ destDir ++ " completed "  ++ (show $ length (qualCounts fileData)) ++ " Base pairs."
        printHtmlStatisticsData (qualCounts fileData) (ord (lc fileData)) destDir -- " " " file: perBaseQualScoresData.js
        putStrLn $ "File: " ++ fname ++ " loaded"
        return $ NGOReadSet (B.pack fname) (ord (lc fileData))

showRead :: Int -> NGLessObject -> BL.ByteString
showRead enc (NGOShortRead a b c) =  BL.pack ((T.unpack a) ++ "\n" ++ (B.unpack b) ++ "\n+\n" ++ (encodeQual enc (B.unpack c)))
showRead _ _ = error "error: The argument must be a read."
