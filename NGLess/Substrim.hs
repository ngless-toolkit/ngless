{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Substrim
    ( substrim
    , subtrimPos
    , removeBps
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Char
import Language


removeBps :: B.ByteString -> (Int,Int) -> B.ByteString
removeBps bps (index,size) = B.take size (B.drop index bps)

substrim :: Int -> NGLessObject -> NGLessObject
substrim cutoff (NGOShortRead rId rS rQ) = NGOShortRead rId (removeBps rS r) (removeBps rQ r)
    where r = subtrimPos rQ (chr cutoff)
substrim _ _ = error "substrim: must have type Int and NGOShortRead"

-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
subtrimPos :: B.ByteString -> Char -> (Int,Int)
subtrimPos quality cutoff = fst $ B.foldl (\a b -> calcSubStrim' a b) ((0,0),(0,0)) quality
    where
        calcSubStrim' :: ((Int,Int), (Int,Int)) -> Char -> ((Int,Int), (Int,Int))
        calcSubStrim' ((i,s),(n_i,n_s)) q = do
            if q >= cutoff
                then (updated, (n_i, n_s + 1))
                else ((i,s)  , (n_s + n_i + 1, 0)) --new start index n_s + n_i + 1
            where updated = if n_s + 1 > s
                                then (n_i, n_s + 1) -- new max
                                else (i,s) -- same max

