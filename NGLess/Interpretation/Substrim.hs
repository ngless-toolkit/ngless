{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Substrim
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

data S4 = S4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
subtrimPos :: B.ByteString -> Char -> (Int,Int)
subtrimPos quality cutoff = case B.foldl' calcSubStrim' (S4 0 0 0 0) quality of
                              S4 i s _ _ -> (i, s)
    where
        calcSubStrim' :: S4 -> Char -> S4
        calcSubStrim' (S4 i s n_i n_s) q
          | q < cutoff = S4 i s (n_s + n_i + 1) 0 --new start index n_s + n_i + 1
          | n_s + 1 > s = S4 n_i (n_s + 1) n_i (n_s + 1)
          | otherwise = S4 i s n_i (n_s + 1)

