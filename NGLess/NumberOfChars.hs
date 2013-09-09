{-# LANGUAGE BangPatterns #-}
module NumberOfChars
    ( countBps
    ) where

addChar (!bpA, !bpC, !bpG, !bpT) 'A' = (bpA + 1, bpC, bpG, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'C' = (bpA, bpC + 1, bpG, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'G' = (bpA, bpC, bpG + 1, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'T' = (bpA, bpC, bpG, bpT + 1)
addChar (!bpA, !bpC, !bpG, !bpT) _ = (bpA, bpC, bpG, bpT)

countChars !c (x:xs) = countChars (addChar c x) xs
countChars !c [] = c 

countBps :: String -> (Integer,Integer,Integer,Integer)
countBps fileContent = countBps' (0,0,0,0) (lines fileContent)
    where
        countBps' c (_id:bps:_optional:_quality:xs) = countBps' (countChars c bps) xs
        countBps' c [] = c
        countBps' _ _ = error "FastQ file does not have a number of lines that is a multiple of 4"
