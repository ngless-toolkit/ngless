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


createBasicStatsJson filePath fileData fname = BL.writeFile filePath resJS        
        where
            res    = basicInfoToJson fname gc' enc' nSeq' sSize'
            resJS  = BL.concat["var basicInfo = [", res, "];"] 
            sSize' = seqSize fileData
            nSeq'  = nSeq fileData
            gc'    = getGCPercent $ bpCounts fileData
            enc'   = getEncoding $ lc fileData


filesProcessedToJson :: FilePath -> T.Text -> IO BL.ByteString
filesProcessedToJson tName script = createFilesProcessed tName script >>= return . encode  

insertFilesProcessedJson :: FilePath -> T.Text -> IO ()
insertFilesProcessedJson t script = do
        jsonPath <- defaultDir >>= return . (</> "filesProcessed.js")
        doesFileExist jsonPath 
          >>= \x -> case x of
            True  -> do
                jsonData <- createFilesProcessed t script
                updateFilesProcessedJson jsonData jsonPath
            False -> do
                jsonData <- filesProcessedToJson t script
                createFilesProcessedJson (BL.concat ["[",jsonData,"]"]) jsonPath

createFilesProcessedJson jsonData jsonPath = BL.writeFile jsonPath $ BL.concat ["var filesProcessed = ", jsonData]

updateFilesProcessedJson jsonData jsonPath = do
    jsonContents <- BL.readFile jsonPath >>= return . snd . BL.splitAt 21 --This is an hack: ('var filesProcessed =' , [jsondata]). This way the operation is allways O(1).
    case decode jsonContents of
        Just a -> createFilesProcessedJson (encode (jsonData : a)) jsonPath
        Nothing -> error ("updateFilesProcessedJson: there was an error with the json file")
