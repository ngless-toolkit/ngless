{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Substrim
    ( substrim
    , subtrimPos
    ) where

import qualified Data.Vector.Unboxed as VU

import Data.Int
import Data.FastQ

substrim :: Int -> ShortRead -> ShortRead
substrim cutoff sr@(ShortRead _ _ rQ) = srSlice s n sr
    where (s,n) = subtrimPos rQ (fromIntegral cutoff)

data S4 = S4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
subtrimPos :: VU.Vector Int8 -> Int8 -> (Int,Int)
subtrimPos quality cutoff = case VU.foldl' calcSubStrim' (S4 0 0 0 0) quality of
                              S4 i s _ _ -> (i, s)
    where
        calcSubStrim' :: S4 -> Int8 -> S4
        calcSubStrim' (S4 i s n_i n_s) q
          | q < cutoff = S4 i s (n_s + n_i + 1) 0 --new start index n_s + n_i + 1
          | n_s + 1 > s = S4 n_i (n_s + 1) n_i (n_s + 1)
          | otherwise = S4 i s n_i (n_s + 1)

