{-# LANGUAGE BangPatterns #-}
module FastQFileData
    (
        Result(..),  iterateFile
    ) where

import Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B

data Result =  Result {bpCounts :: !(Int, Int, Int, Int) , lc :: !Char, qualCounts :: ![Map Char Int], nSeq :: !Int, seqSize :: !(Int,Int)} deriving(Show)

--- Constants
maxBPPossible = 126
---

-- mapAddFunction :: To increase the old_value value by 1 of a given key.
mapAddFunction _ new_value old_value = old_value + new_value

-- addEachCount :: Update Map counts with a new quality.
addEachCount :: Char -> Map Char Int -> Map Char Int
addEachCount qual counts = insertWithKey mapAddFunction qual 1 counts

addToCount counts qual = addToCount' counts qual []
        where
            addToCount' (counts:xs) (q:ys) qualResults = addToCount' xs ys ((addEachCount q counts) : qualResults)
            addToCount' _ [] qualResults = reverse qualResults

addChar !(!bpA, !bpC, !bpG, !bpT) 'A' = (bpA + 1, bpC, bpG, bpT)
addChar !(!bpA, !bpC, !bpG, !bpT) 'C' = (bpA, bpC + 1, bpG, bpT)
addChar !(!bpA, !bpC, !bpG, !bpT) 'G' = (bpA, bpC, bpG + 1, bpT)
addChar !(!bpA, !bpC, !bpG, !bpT) 'T' = (bpA, bpC, bpG, bpT + 1)
addChar !(!bpA, !bpC, !bpG, !bpT) _ = (bpA, bpC, bpG, bpT)

countChars c (x:xs) = countChars (addChar c x) xs
countChars c [] = c


seqMinMax :: (Int,Int) -> Int -> (Int,Int)
seqMinMax !(!minSeq, !maxSeq) !length =  ((min length minSeq),(max length maxSeq))

--updateResults :: Used to fill in the structure "Result" with the FastQ file info.
updateResults :: Result -> String -> String -> Result
updateResults fileData seq qual = Result (countChars (bpCounts fileData) seq)
                                         (Prelude.foldr min (lc fileData) qual)
                                         (addToCount (qualCounts fileData) qual)
                                         ((nSeq fileData) + 1)
                                         (seqMinMax (seqSize fileData) (length seq))

--iterateFile :: Used to iterate the file in a strict manner.
iterateFile :: B.ByteString -> Result
iterateFile contents = iterateFile' initial (B.lines contents)
        where
                initial = Result (0,0,0,0) '~' (replicate maxBPPossible (fromList [] :: Map Char Int)) 0 (maxBound :: Int, minBound :: Int)
                iterateFile' !r (_:seq:_:quals:xs) =
                        iterateFile' (updateResults r (B.unpack seq) (B.unpack quals)) xs
                iterateFile' !r [] = r

