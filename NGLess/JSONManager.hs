{-# LANGUAGE OverloadedStrings #-}

module JSONManager
    (
        createBasicStatsJson,
        insertFilesProcessedJson,
        insertCountsProcessedJson
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Control.Applicative ((<$>))
import Data.Aeson
import System.FilePath.Posix

import Configuration
import Data.Json
import Data.FastQ
import qualified FastQStatistics as FQ
import FileManagement


fProc = "var filesProcessed = "
cProc = "var countsProcessed = "

createBasicStatsJson :: FQ.Result -> FilePath -> FastQEncoding -> BL.ByteString
createBasicStatsJson fileData fname enc = resJS
        where
            res    = basicInfoToJson fname gc' enc' nSeq' sSize'
            resJS  = BL.concat["var basicInfo = [", res, "];"] 
            sSize' = FQ.seqSize fileData
            nSeq'  = FQ.nSeq fileData
            gc'    = FQ.gcFraction fileData
            enc'   = encodingName enc


insertFilesProcessedJson :: FilePath -> T.Text -> IO ()
insertFilesProcessedJson t script = do
        jsonPath <- (</> "filesProcessed.js") <$> outputDirectory t
        doesFileExist jsonPath 
          >>= \x -> case x of
            True  -> createFilesProcessed t script >>= \v -> updateProcessedJson fProc v jsonPath 21
            False -> filesProcessedToJson t script >>= \v -> createProcessedJson fProc (BL.concat ["[", v, "]"]) jsonPath


filesProcessedToJson :: FilePath -> T.Text -> IO BL.ByteString
filesProcessedToJson tName script = encode <$> createFilesProcessed tName script


insertCountsProcessedJson :: FilePath -> IO ()
insertCountsProcessedJson fp = do
        jsonPath <- (</> "countsProcessed.js") <$> outputDirectory fp
        exists <- doesFileExist jsonPath
        if exists
            then updateProcessedJson cProc jsonData jsonPath 22
            else createProcessedJson cProc (BL.concat ["[",encode jsonData,"]"]) jsonPath
    where
        jsonData = createCountsProcessed fp


updateProcessedJson v jsonData jsonPath k = do
    jsonContents <- BL.readFile jsonPath >>= return . snd . BL.splitAt k --This is an hack: ('var varname =' , [jsondata]). This way the operation is allways O(1).
    case decode jsonContents of
        Just a -> createProcessedJson v (encode (jsonData : a)) jsonPath
        Nothing -> error ("updateProcessedJson: there was an error with the json file")

createProcessedJson vname jsonData jsonPath = BL.writeFile jsonPath $ BL.concat [vname, jsonData]
