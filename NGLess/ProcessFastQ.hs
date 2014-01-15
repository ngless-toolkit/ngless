module ProcessFastQ
    (
    readFastQ,
    readReadSet,
    removeFileIfExists,
    getTempFilePath
    ) where
    
import qualified Data.ByteString.Lazy.Char8 as GZipReader
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Codec.Compression.GZip as GZip    
import Data.Text
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
    res <- openTempFile (fst oldFilePath) (snd oldFilePath)   
    return (fst res)

-- Uncompression of a given fastQ file if it's compressed in a .gz format.
unCompress fname =
    if isInfixOf (pack ".gz") (pack fname)
        then fmap GZip.decompress (GZipReader.readFile fname)
        else GZipReader.readFile fname -- not compressed


-- Removes the destiny directory if it already exists from previous executions.
createDir destDir = do
    doesDirExist <- doesDirectoryExist destDir
    when (doesDirExist) $ removeDirectoryRecursive destDir
    createDirectory destDir


readReadSet :: B.ByteString -> IO [NGLessObject]
readReadSet fileName = do
    fileContents' <- (B.readFile (B.unpack fileName))
    putStrLn ("FileName: " ++ B.unpack fileName)
    readReadSet' (Prelude.map (B.unpack) (B.lines fileContents')) []
    where readReadSet' (readId:readSeq:_:readQual:xs) res =  readReadSet' xs ((NGOShortRead (T.pack readId) (B.pack readSeq) readQual) : res)
          readReadSet' [] res = return res
          readReadSet' _ _ = error "Number of lines is not multiple of 4!"

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

removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp
