{-# LANGUAGE OverloadedStrings #-}

module Data.Json
    (
        BasicInfo(..),
        FilesProcessed(..),
        CountsProcessed(..),
        basicInfoToJson,
        createFilesProcessed,
        createCountsProcessed
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson

import System.Time

import Control.Applicative
import Control.Monad

data BasicInfo = BasicInfo String Double String Int (Int,Int) deriving (Show)
data FilesProcessed = FilesProcessed String String T.Text deriving (Show, Eq)
data CountsProcessed = CountsProcessed String deriving (Show, Eq)


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


instance ToJSON CountsProcessed where
   toJSON (CountsProcessed a) = object [ "fp" .= a ]

instance FromJSON CountsProcessed where
    parseJSON (Object v) = CountsProcessed <$>
                            v .: "fp"
    parseJSON _          = mzero


basicInfoToJson :: String -> Double -> String -> Int -> (Int,Int) -> BL.ByteString
basicInfoToJson fname gc enc numSeq seqL = encode $ BasicInfo fname gc enc numSeq seqL


createFilesProcessed :: String -> T.Text -> IO FilesProcessed
createFilesProcessed template script = getClockTime >>= \x -> return $ FilesProcessed template (show x) script

createCountsProcessed :: String -> CountsProcessed
createCountsProcessed fp = CountsProcessed fp
