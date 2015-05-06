{-# LANGUAGE OverloadedStrings #-}

module Data.Json
    ( FilesProcessed(..)
    , CountsProcessed(..)
    , createFilesProcessed
    ) where

import qualified Data.Text as T

import Data.Aeson

import System.Time

import Control.Applicative
import Control.Monad

data FilesProcessed = FilesProcessed String String T.Text deriving (Show, Eq)
data CountsProcessed = CountsProcessed String deriving (Show, Eq)

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



createFilesProcessed :: String -> T.Text -> IO FilesProcessed
createFilesProcessed template script = getClockTime >>= \x -> return $ FilesProcessed template (show x) script
