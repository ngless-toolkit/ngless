{-# LANGUAGE BangPatterns #-}
module FPreProcess
    (
        FPreProcess.Read(..), substrim,calculateSubStrim
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Char
import Language

data Read = Read {readId :: String, seq :: String, qual::String} deriving (Show,Eq, Prelude.Read)

removeBps :: [a] -> (Int,Int) -> [a]
removeBps bps (index,size) = take size (drop index bps)

--TODO: Discuss how to calculate the quality. This is not totally implemented!
substrim :: Int -> NGLessObject -> NGLessObject
substrim cutoff (NGOShortRead rId rSeq rQual) = do
    let res = calculateSubStrim (fmap ord rQual) cutoff
    NGOShortRead rId (B.pack (removeBps (B.unpack rSeq) res)) (removeBps rQual res)

substrim _ _ = error "substrim: must have type Int and NGOShortRead"

-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
calculateSubStrim :: [Int] -> Int -> (Int,Int)
calculateSubStrim quality cutoff = calculateSubStrim' quality 0 0 0 (0,0)
    where   calculateSubStrim' [] _ _ _ (index,size) = (index,size)
            calculateSubStrim' (q:xs) inc offset act_index (index, size) =
                (if q >= cutoff
                    then calculateSubStrim' xs (inc + 1) (offset + 1) act_index (if (inc + 1) > size
                                                                         then (act_index , (1 + size))
                                                                         else (index, size))
                    else calculateSubStrim' xs 0 (offset + 1) (offset + 1) (index,size))
