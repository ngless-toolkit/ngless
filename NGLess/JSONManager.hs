{-# LANGUAGE OverloadedStrings #-}

module JSONManager
    (
        basicInfoToJson
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Applicative
import Control.Monad

import Data.Aeson

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
basicInfoToJson fname gc enc numSeq seqL = do
    let bi = BasicInfo fname gc enc numSeq seqL
    encode bi