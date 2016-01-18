{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Substrim
    ( substrim
    , subtrimPos
    , cutByteString
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Char
import Data.FastQ


cutByteString :: B.ByteString -> (Int,Int) -> B.ByteString
cutByteString bps (index,size) = B.take size (B.drop index bps)

substrim :: Int -> ShortRead -> ShortRead
substrim cutoff (ShortRead rId rS rQ) = ShortRead rId (cutByteString rS r) (cutByteString rQ r)
    where r = subtrimPos rQ (chr cutoff)

-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
subtrimPos :: B.ByteString -> Char -> (Int,Int)
subtrimPos quality cutoff = fst $ B.foldl' calcSubStrim' ((0,0),(0,0)) quality
    where
        calcSubStrim' :: ((Int,Int), (Int,Int)) -> Char -> ((Int,Int), (Int,Int))
        calcSubStrim' ((!i,!s),(!n_i,!n_s)) q =
            if q >= cutoff
                then (updated, (n_i, n_s + 1))
                else ((i,s)  , (n_s + n_i + 1, 0)) --new start index n_s + n_i + 1
            where updated = if n_s + 1 > s
                                then (n_i, n_s + 1) -- new max
                                else (i,s) -- same max

