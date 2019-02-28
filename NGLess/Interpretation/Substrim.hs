{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Substrim
    ( substrim
    , endstrim
    , smoothtrim
    , EndstrimEnds(..)
    , subtrimPos
    , endstrimPos
    , smoothtrimPos
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

-- |Run a sliding window across the read and cut when average quality in given window
--  drops below the requested value.
smoothtrim :: Int -> Int -> ShortRead -> ShortRead
smoothtrim window cutoff sr@(ShortRead _ _ rQ) = srSlice start numbases sr
    where (start,numbases) = smoothtrimPos window rQ (toEnum cutoff)

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


smoothtrimPos :: Int -> VS.Vector Int8 -> Int8 -> (Int, Int)
smoothtrimPos window quality cutoff = if VS.null quality
                                         then (0, 0)
                                         else subtrimPos smoothedQuality cutoff
    where
        qHead = VS.head quality
        qLast = VS.last quality

        -- We pad the original quality values left and right by repeating
        -- the last value on each edge. this allows averaging the quality
        -- of each position taking into account values of surrounding
        -- bases given the specified window

        -- For a window of size n we need to pad n-1 bases divided between
        -- both ends of the read
        -- With even numbers the left pad is 1 unit smaller than the right
        pad = window - 1
        left_pad = pad `div` 2
        right_pad = pad - left_pad

        quals = VS.replicate left_pad qHead VS.++ quality VS.++ VS.replicate right_pad qLast

        -- |Computes mean of a sliding window from starting position to user-specified window size
        meanSlideWindow :: Int -> Int8
        meanSlideWindow start = intRoundMean (VS.slice start window quals) window

        -- If we compute the mean of the window with Int8 we will overflow easily
        -- and to avoid integer division that always floors we work with doubles
        -- and round back to Int8 afterwards
        intRoundMean :: VS.Vector Int8 -> Int -> Int8
        intRoundMean q w = round $ (fromIntegral $ VS.sum $ VS.map fromEnum q) / (fromIntegral w :: Double)

        -- Finally we smooth the quality values on each window and create a new
        -- quality vector that will be passed to subtrimPos
        smoothedQuality = VS.unfoldrN (VS.length quality) (\n -> Just (meanSlideWindow n, n+1)) 0
