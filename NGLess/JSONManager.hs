{-# LANGUAGE OverloadedStrings #-}

module JSONManager
    (
        createBasicStatsJson,
        insertFilesProcessedJson
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Data.Aeson
import System.FilePath.Posix

import PrintFastqBasicStats
import FastQFileData
import FileManagement

import Data.DefaultValues
import Data.Json


createBasicStatsJson filePath fileData fname = do
        let res = basicInfoToJson fname gc' enc' (nSeq fileData) (seqSize fileData) 
            resJS = BL.concat["var basicInfo = [", res, "];"]
        BL.writeFile filePath resJS
        where 
            gc' = getGCPercent $ bpCounts fileData
            enc' = getEncoding $ lc fileData


filesProcessedToJson :: FilePath -> T.Text -> IO BL.ByteString
filesProcessedToJson templateName script = do
    fp <- createFilesProcessed templateName script 
    return $ encode fp  

insertFilesProcessedJson :: FilePath -> T.Text -> IO ()
insertFilesProcessedJson template script = do
        defaultDir' <- defaultDir
        let jsonPath = defaultDir' </> "filesProcessed.js"
        doesExist <- doesFileExist jsonPath
        case doesExist of
            True  -> do
                jsonData <- createFilesProcessed template script
                updateFilesProcessedJson jsonData jsonPath
            False -> do
                jsonData <- filesProcessedToJson template script
                createFilesProcessedJson (BL.concat ["[",jsonData,"]"]) jsonPath

createFilesProcessedJson jsonData jsonPath = BL.writeFile jsonPath $ BL.concat ["var filesProcessed = ", jsonData]

updateFilesProcessedJson jsonData jsonPath = do
    contents <- BL.readFile jsonPath
    let jsonContents = snd $ BL.splitAt 21 contents --This is an hack: ('var filesProcessed =' , [jsondata]). This way the operation is allways O(1).
        res = decode jsonContents :: Maybe [FilesProcessed]
    case res of
        Just a -> createFilesProcessedJson (encode (jsonData : a)) jsonPath
        Nothing -> error ("updateFilesProcessedJson: there was an error with the json file")