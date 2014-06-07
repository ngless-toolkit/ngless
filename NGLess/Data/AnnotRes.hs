{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.AnnotRes
    ( 
      GffCount(..), 
      showGffCount
    ) where

import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.GFF

data GffCount = GffCount L8.ByteString GffType !Int deriving Show

showGffCount :: [GffCount] -> BL.ByteString 
showGffCount = BL.unlines . fmap showCounts

showCounts :: GffCount -> BL.ByteString
showCounts (GffCount s t c) = BL.fromChunks [s, "\t" , showType t, "\t", encode c]
    where encode = L8.pack . show --could be used Data.Binary (encode)
