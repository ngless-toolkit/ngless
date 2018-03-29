{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Substrim
    ( substrim
    , endstrim
    , EndstrimEnds(..)
    , subtrimPos
    , endstrimPos
    ) where

import qualified Data.Vector.Storable as VS

import Data.Maybe
import Data.Int

import Data.FastQ

data EndstrimEnds = EndstrimBoth | Endstrim3 | Endstrim5
    deriving (Show, Enum, Eq)

substrim :: Int -> ShortRead -> ShortRead
substrim cutoff sr@(ShortRead _ _ rQ) = srSlice s n sr
    where (s,n) = subtrimPos rQ (toEnum cutoff)

endstrim :: EndstrimEnds -> Int -> ShortRead -> ShortRead
endstrim ends cutoff sr@(ShortRead _ _ rQ) = srSlice s n sr
    where (s,n) = endstrimPos ends rQ (toEnum cutoff)

data S4 = S4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
subtrimPos :: VS.Vector Int8 -> Int8 -> (Int,Int)
subtrimPos quality cutoff = case VS.foldl' calcSubStrim' (S4 0 0 0 0) quality of
                              S4 i s _ _ -> (i, s)
    where
        calcSubStrim' :: S4 -> Int8 -> S4
        calcSubStrim' (S4 i s n_i n_s) q
          | q < cutoff = S4 i s (n_s + n_i + 1) 0 --new start index n_s + n_i + 1
          | n_s + 1 > s = S4 n_i (n_s + 1) n_i (n_s + 1)
          | otherwise = S4 i s n_i (n_s + 1)

endstrimPos :: EndstrimEnds -> VS.Vector Int8 -> Int8 -> (Int, Int)
endstrimPos method quality cutoff = (start, trim3p $ VS.drop start quality)
    where
        start
            | do5 = fromMaybe len $ VS.findIndex (>= cutoff) quality
            | otherwise = 0
        do5 = method `elem` [Endstrim5, EndstrimBoth]
        do3 = method `elem` [Endstrim3, EndstrimBoth]
        len = VS.length quality
        trim3p qs
            | do3 = trim3p' (VS.length qs) qs
            | otherwise = VS.length qs
        trim3p' :: Int -> VS.Vector Int8 -> Int
        trim3p' 0 _ = 0
        trim3p' n qs
            | qs VS.! (n - 1) >= cutoff = n
            | otherwise = trim3p' (n - 1) qs


