{-# LANGUAGE OverloadedStrings #-}

module Data.Json
    (
        BasicInfo(..),
        FilesProcessed(..),
        basicInfoToJson,
        createFilesProcessed
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson

import System.Time

import Control.Applicative
import Control.Monad

data BasicInfo = BasicInfo String Double String Int (Int,Int) deriving (Show)
data FilesProcessed = FilesProcessed String String T.Text deriving (Show)


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


basicInfoToJson :: String -> Double -> String -> Int -> (Int,Int) -> BL.ByteString
basicInfoToJson fname gc enc numSeq seqL = encode $ BasicInfo fname gc enc numSeq seqL


createFilesProcessed :: String -> T.Text -> IO FilesProcessed
createFilesProcessed template script = do
    currentTime <- getClockTime
    return $ FilesProcessed template (show currentTime) script
