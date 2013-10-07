{-# LANGUAGE BangPatterns #-}
module FPreProcess
    (
        FPreProcess.Read(..), substrim,calculateSubStrim
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Char
import Language

data Read = Read {readId :: String, seq :: String, qual::String} deriving (Show,Eq, Prelude.Read)

removeBps :: B.ByteString -> (Int,Int) -> B.ByteString
removeBps bps (index,size) = B.take size . B.drop index $ bps

--TODO: Discuss how to calculate the quality. This is not totally implemented!
substrim :: Int -> NGLessObject -> NGLessObject
substrim cutoff (NGOShortRead readId readSeq readQual) = do
    let res = calculateSubStrim (map ord (B.unpack readSeq)) cutoff
    NGOShortRead readId (removeBps readSeq res) (removeBps readQual res)

substrim _ _ = error "substrim: must have type Int and NGOShortRead"

-- Receives a Quality array and returns a pair with the index and size of the subsequence which has the most consecutive bps respecting the cutoff.
calculateSubStrim :: [Int] -> Int -> (Int,Int)
calculateSubStrim qual cutoff = calculateSubStrim' qual 0 0 0 (0,0)
    where   calculateSubStrim' [] _ _ _ (index,size) = (index,size)
            calculateSubStrim' (q:xs) inc offset act_index (index, size) =
                (if q >= cutoff
                    then calculateSubStrim' xs (inc + 1) (offset + 1) act_index (if (inc + 1) > size
                                                                         then (act_index , (1 + size))
                                                                         else (index, size))
                    else calculateSubStrim' xs 0 (offset + 1) (offset + 1) (index,size))


