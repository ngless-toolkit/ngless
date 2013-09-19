module ProcessFastQ
    (
		readFastQ
    ) where
    
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.GZip as GZip    
import Data.Text
import Data.Char

import System.Directory
import Control.Monad
import PerBaseQualityScores
import PrintFastqBasicStats
import FastQFileData
    
-- Uncompression of a given fastQ file if it's compressed in a .gz format.
unCompress fname =
    if isInfixOf (pack ".gz") (pack fname)
        then fmap GZip.decompress (B.readFile fname)
        else B.readFile fname -- not compressed


-- Removes the destiny directory if it already exists from previous executions.
createDir destDir = do
    doesDirExist <- doesDirectoryExist destDir
    when (doesDirExist) $ removeDirectoryRecursive destDir
    createDirectory destDir

readFastQ :: FilePath -> IO ()
readFastQ fname = do
        contents <- unCompress fname
        let fileData = iterateFile contents
            destDir = (fname ++ "_ngless")
        createDir destDir
        copyFile "Html/index.html" (destDir ++ "/index.html")
        copyFile "Html/perBaseQualScores.css" (destDir ++ "/perBaseQualScores.css")
        printHtmlBasicStats destDir fileData fname
        printHtmlStatisticsData (qualCounts fileData) (ord (lc fileData)) destDir
        printHtmlEndScripts (destDir ++ "/index.html")
