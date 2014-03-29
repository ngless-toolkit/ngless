{-# LANGUAGE OverloadedStrings #-}

module JSONManager
    (
        createBasicStatsJson,
        basicInfoToJson,
        insertFilesProcessedJson
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Control.Applicative
import Control.Monad

import PrintFastqBasicStats
import FastQFileData
import FileManagement

import Data.Aeson

import System.Time
import System.FilePath.Posix

data BasicInfo = BasicInfo String Double String Int (Int,Int) deriving (Show)

instance ToJSON BasicInfo where
   toJSON (BasicInfo a b c d e) = object ["fileName" .= a,
                                        "GC" .= b,
                                        "Encoding" .= c,
                                        "NumSeqs" .= d,
                                        "SeqLength" .= e]

instance FromJSON BasicInfo where
    parseJSON (Object v) = BasicInfo <$>
                            v .: "fileName" <*>
                            v .: "GC" <*>
                            v .: "Encoding" <*>
                            v .: "NumSeqs" <*>
                            v .: "SeqLength"
    parseJSON _          = mzero


basicInfoToJson :: String -> Double -> String -> Int -> (Int,Int) -> BL.ByteString
basicInfoToJson fname gc enc numSeq seqL = encode $ BasicInfo fname gc enc numSeq seqL

createBasicStatsJson filePath fileData fname = do
        let res = basicInfoToJson fname gc' enc' (nSeq fileData) (seqSize fileData) 
            resJS = BL.concat["var basicInfo = [", res, "];"]
        BL.writeFile filePath resJS
        where 
            gc' = getGCPercent $ bpCounts fileData
            enc' = getEncoding $ lc fileData

data FilesProcessed = FilesProcessed String String T.Text deriving (Show)

instance ToJSON FilesProcessed where
   toJSON (FilesProcessed a b c) = object [ "name" .= a,
                                            "time" .= b,
                                            "script" .=c ]

instance FromJSON FilesProcessed where
    parseJSON (Object v) = FilesProcessed <$>
                            v .: "name" <*>
                            v .: "time" <*>
                            v .: "script"
    parseJSON _          = mzero

createFilesProcessed :: String -> T.Text -> IO FilesProcessed
createFilesProcessed template script = do
    currentTime <- getClockTime
    return $ FilesProcessed template (show currentTime) script

filesProcessedToJson :: FilePath -> T.Text -> IO BL.ByteString
filesProcessedToJson templateName script = do
    fp <- createFilesProcessed templateName script 
    return $ encode fp  

insertFilesProcessedJson :: FilePath -> T.Text -> IO ()
insertFilesProcessedJson template script = do
        defaultDir' <- defaultDir
        let jsonPath = defaultDir' </> "filesProcessed.js"
        doesExist <- doesFileExist jsonPath
        putStrLn (show doesExist)
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
    _ <- printNglessLn (show res)   
    case res of
        Just a -> createFilesProcessedJson (encode (jsonData : a)) jsonPath
        Nothing -> error ("updateFilesProcessedJson: there was an error with the json file")