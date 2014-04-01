{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


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
import qualified Data.ByteString.Char8 as B

import qualified Codec.Compression.GZip as GZip    
import qualified Data.Map as Map
import Data.Char

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Monad
import System.FilePath.Posix

import SamBamOperations
import FileManagement
import PerBaseQualityScores
import FastQFileData
import Language
import JSONManager    

-- Uncompression of a given fastQ file if it's compressed in a .gz format.
unCompress fname =
    if T.isInfixOf (T.pack ".gz") (T.pack fname)
        then fmap GZip.decompress (BL.readFile fname)
        else BL.readFile fname -- not compressed

appendFile' = BL.hPutStrLn


getNGOString (Just (NGOString s)) = s
getNGOString _ = error "Error: Type is different of String"

elFromMap el args = Map.lookup (T.pack el) (Map.fromList args)


writeToUncFile (NGOMappedReadSet path) newfp = do
    let path' = T.unpack path
    contents' <- readPossiblyCompressedFile (B.pack path') 
    write (T.unpack newfp) $ contents' 
    return $ NGOMappedReadSet newfp

writeToUncFile (NGOReadSet path enc tmplate) newfp = do
    let newfp' = T.unpack newfp
    contents' <- readPossiblyCompressedFile path
    write newfp' $ contents' 
    return $ NGOReadSet (B.pack newfp') enc tmplate

writeToUncFile err _ = error ("writeToUncFile: Should have received a NGOReadSet, but the type was: " ++ (show err))

writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject   
writeToFile (NGOList el) args = do
    let templateFP = getNGOString ( elFromMap "ofile" args )
        indexFPs = map (T.pack . show) [1..(length el)]
        newFPS' = map (\x -> T.replace (T.pack "{index}") x templateFP) indexFPs
    res <- zipWithM (\x y -> writeToUncFile x y) el newFPS'
    return (NGOList res)

writeToFile el@(NGOReadSet _ _ _) args = do
    let newfp = getNGOString ( elFromMap "ofile" args )
    writeToUncFile el newfp

writeToFile el@(NGOMappedReadSet fp) args = do
    let map' = Map.fromList args
        newfp = getNGOString ( Map.lookup (T.pack "ofile") map') --
        format = Map.lookup (T.pack "format") map'
    case format of
        Nothing -> writeToUncFile el newfp
        Just x -> case x of 
            (NGOSymbol "bam") -> samToBam (T.unpack fp) (T.unpack newfp) --newfp will contain the bam
            _     -> writeToUncFile el newfp 

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

write :: String -> BL.ByteString -> IO ()
write fp contents = BL.writeFile fp contents 

readPossiblyCompressedFile ::  B.ByteString -> IO BL.ByteString
readPossiblyCompressedFile fileName = unCompress (B.unpack fileName)

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
