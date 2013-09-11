{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.GZip as GZip

import Language
import FastQFileData
import Data.Text as T
import Data.Char
import PerBaseQualityScores
import PrintFastqBasicStats
import System.Directory
import Control.Monad


interpret :: [(Int,Expression)] -> IO ()
interpret [] = return ()
interpret ((_,e):es) = interpret' e >> interpret es

interpret' :: Expression -> IO ()
interpret' (Sequence es) = handleSequence es
interpret' (Assignment var func) = variableAssignment var >> interpretFunctions func
interpret' e = error (Prelude.concat ["interpret: cannot handle ", show e])

variableAssignment (Variable varName) = print varName
interpretFunctions (FunctionCall functionType (ConstStr fname) _exprs _block) =
    case functionType of
        Ffastq -> readFastQ (T.unpack fname)
        _ -> print functionType -- all the other functionCalls
interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"



-- functions to handle interpretation

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

handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret' e >> handleSequence es
handleSequence [] = return ()

