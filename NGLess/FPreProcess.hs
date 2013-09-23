module FPreProcess
    (
        FPreProcess.Read(..), substrim
    ) where

import Data.Char

data Read = Read {id :: String, seq :: String, qual::String} deriving (Show,Eq)

removeBps :: String -> (Int,Int) -> String
removeBps bps (index,size) = take size (drop index bps)

--TODO: Discuss how to calculate the quality. This is not totally implemented!
substrim :: Int -> FPreProcess.Read -> FPreProcess.Read
substrim cutoff eachRead = do
    let res = calculateSubStrim (map ord (qual eachRead)) cutoff
    FPreProcess.Read (FPreProcess.id eachRead) (removeBps (FPreProcess.seq eachRead) res) (removeBps (qual eachRead) res)

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


