module ProcessFastQ
    (
		readFastQ,
        createReadSet,
        removeFileIfExists
    ) where
    
import qualified Data.ByteString.Lazy.Char8 as GZipReader
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Codec.Compression.GZip as GZip    
import Data.Text
import Data.Char

import System.Directory
import Control.Monad
import PerBaseQualityScores
import PrintFastqBasicStats
import FastQFileData
import Language
    


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


createReadSet :: B.ByteString -> IO [NGLessObject]
createReadSet fileName = do
    fileContents' <- (B.readFile (B.unpack fileName))
    putStrLn ("FileName: " ++ B.unpack fileName)
    createReadSet' (Prelude.map (B.unpack) (B.lines fileContents')) []
    where createReadSet' (readId:readSeq:_:readQual:xs) res =  createReadSet' xs ((NGOShortRead (T.pack readId) (B.pack readSeq) (B.pack readQual)) : res)
          createReadSet' [] res = return res
          createReadSet' _ _ = error "Number of lines is not multiple of 4!"

readFastQ :: FilePath -> IO NGLessObject
readFastQ fname = do
        contents <- unCompress fname
        let fileData = iterateFile contents
            destDir = (fname ++ "_ngless")
        createDir destDir
        copyFile "Html/index.html" (destDir ++ "/index.html")
        copyFile "Html/perBaseQualScores.css" (destDir ++ "/perBaseQualScores.css")
        copyFile "Html/perBaseQualityScores.js" (destDir ++ "/perBaseQualityScores.js")
        createBasicStatsJson (destDir ++ "/basicStats.js") fileData fname -- generate JSON DATA file: basicStats.js
        printHtmlStatisticsData (qualCounts fileData) (ord (lc fileData)) destDir -- " " " file: perBaseQualScoresData.js
        return $ NGOReadSet (B.pack fname)

removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp
