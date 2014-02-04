{-# LANGUAGE BangPatterns #-}

module ProcessFastQ
    (
    readFastQ,
    readReadSet,
    removeFileIfExists,
    getTempFilePath,
    writeToFile,
    showRead,
    unCompress,
    writeGZIP,
    writeReadSet
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Codec.Compression.GZip as GZip    
import qualified Data.Map as Map

import qualified Data.Text as T
import Data.Char

import System.Directory
import System.IO
import System.FilePath.Posix

import Control.Monad

import PerBaseQualityScores
import PrintFastqBasicStats
import FastQFileData
import Language
    



getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let oldFilePath = splitFileName fp
    res <- openTempFile (fst oldFilePath) (snd oldFilePath ++ ".gz")   
    return (fst res)

-- Uncompression of a given fastQ file if it's compressed in a .gz format.
unCompress fname =
    if T.isInfixOf (T.pack ".gz") (T.pack fname)
        then fmap GZip.decompress (BL.readFile fname)
        else BL.readFile fname -- not compressed


getNGOString (Just (NGOString s)) = T.unpack s
getNGOString _ = "Error: Type is different of String"

writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject   
writeToFile (NGOReadSet path enc) args = do
    let map' = Map.fromList args
        destFilePath = Map.lookup (T.pack "ofile") map'
        newfp = getNGOString destFilePath
    copyFile (B.unpack path) newfp
    return $ NGOReadSet (B.pack newfp) enc

writeToFile _ _ = error "Error: writeToFile Not implemented yet"



writeReadSet :: B.ByteString -> [NGLessObject] -> IO FilePath
writeReadSet fn rs = do
    newfp <- getTempFilePath (B.unpack fn)
    removeFileIfExists newfp
    writeGZIP newfp $ parseNGOReadSet rs
    return newfp

parseNGOReadSet :: [NGLessObject] -> BL.ByteString
parseNGOReadSet = BL.unlines . (fmap showRead)

writeGZIP :: String -> BL.ByteString -> IO ()
writeGZIP fp contents = BL.writeFile fp $ GZip.compress contents 

readPossiblyCompressedFile ::  B.ByteString -> IO BL.ByteString
readPossiblyCompressedFile fileName = unCompress (B.unpack fileName)

readReadSet :: B.ByteString -> IO [NGLessObject]
readReadSet fn = parseReadSet `fmap` (readPossiblyCompressedFile fn)

parseReadSet :: BL.ByteString -> [NGLessObject]
parseReadSet = parse' . (fmap BL.unpack) . BL.lines
        where
            parse' [] = []
            parse' xs = (createRead (Prelude.take 4 xs) : parse' (Prelude.drop 4 xs))
            createRead :: [String] -> NGLessObject
            createRead r = case (Prelude.length r) of
                4 -> NGOShortRead (T.pack $ r !! 0) (B.pack $ r !! 1) (B.pack $ r !! 3)
                _ -> error "Number of lines is not multiple of 4!"

decodeQuality enc = fmap (chr . subtract enc . ord)


readFastQ :: FilePath -> IO NGLessObject
readFastQ fname = do
        contents <- unCompress fname
        let fileData = iterateFile contents
            destDir = (fname ++ "_ngless")
        createDir destDir 
        copyFile "Html/index.html" (destDir ++ "/index.html")
        copyFile "Html/perBaseQualScores.css" (destDir ++ "/perBaseQualScores.css")
        copyFile "Html/perBaseQualityScores.js" (destDir ++ "/perBaseQualityScores.js")
        putStrLn $ "Generation of statistics for " ++ fname
        createBasicStatsJson (destDir ++ "/basicStats.js") fileData fname -- generate JSON DATA file: basicStats.js
        putStrLn $ "Simple Statistics for: " ++ fname ++ " completed"
        printHtmlStatisticsData (qualCounts fileData) (ord (lc fileData)) destDir -- " " " file: perBaseQualScoresData.js
        putStrLn $ "File: " ++ fname ++ " loaded"
        return $ NGOReadSet (B.pack fname) (ord (lc fileData))

showRead :: NGLessObject -> BL.ByteString
showRead (NGOShortRead a b c) =  BL.pack ((T.unpack a) ++ "\n" ++ "+??\n" ++ (B.unpack b) ++ "\n" ++ (B.unpack c) ++ "\n")
showRead _ = error "error: The argument must be a read."

removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp

-- Removes the destiny directory if it already exists from previous executions.
createDir destDir = do
    doesDirExist <- doesDirectoryExist destDir
    when (doesDirExist) $ removeDirectoryRecursive destDir
    createDirectory destDir
