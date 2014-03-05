{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FPreProcess
    ( substrim
    , calculateSubStrim
    , removeBps
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Char
import Language


removeBps :: B.ByteString -> (Int,Int) -> B.ByteString
removeBps bps (index,size) = B.take size (B.drop index bps)

substrim :: Int -> NGLessObject -> NGLessObject
substrim cutoff (NGOShortRead rId rSeq rQual) = do
    let res = calculateSubStrim rQual (chr cutoff)
    NGOShortRead rId (removeBps rSeq res) (removeBps rQual res)

substrim _ _ = error "substrim: must have type Int and NGOShortRead"

-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
calculateSubStrim :: B.ByteString -> Char -> (Int,Int)
calculateSubStrim quality cutoff = fst $ B.foldl (\a b -> calcSubStrim' cutoff a b) ((0,0),(0,0)) quality
    
calcSubStrim' :: Char -> ((Int,Int), (Int,Int)) -> Char -> ((Int,Int), (Int,Int))    
calcSubStrim' cutoff ((i,s),(n_i,n_s)) q = do
    if q >= cutoff
        then (updateIndexs' , (n_i, n_s + 1))
        else ((i,s)         , (n_s + n_i + 1, 0)) --new start index n_s + n_i + 1
    where updateIndexs' = if (n_s + 1 > s)
                            then (n_i, n_s + 1) -- new max
                            else (i,s) -- same max

