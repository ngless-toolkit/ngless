{-# LANGUAGE BangPatterns #-}
module NumberOfChars
    ( 
	countBps,
	lowestChar
    ) where

addChar (!bpA, !bpC, !bpG, !bpT) 'A' = (bpA + 1, bpC, bpG, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'C' = (bpA, bpC + 1, bpG, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'G' = (bpA, bpC, bpG + 1, bpT)
addChar (!bpA, !bpC, !bpG, !bpT) 'T' = (bpA, bpC, bpG, bpT + 1)
addChar (!bpA, !bpC, !bpG, !bpT) _ = (bpA, bpC, bpG, bpT)

countChars !c (x:xs) = countChars (addChar c x) xs
countChars !c [] = c

countBps :: (Num t3, Num t2, Num t1, Num t) => String -> (t3, t2, t1, t)
countBps fileContent = countBps' (0,0,0,0) (lines fileContent)
	where
		countBps' c (id:sequence:optional:quality:xs) = countBps' (countChars c sequence) xs
		countBps' c [] = c	

lowestChar :: String -> Char
lowestChar fileContent = lowestChar' '~' (lines fileContent)
		where
			lowestChar' lc (id:sequence:optional:quality:xs) = lowestChar' (min (minimum quality) lc)  xs
			lowestChar' lc [] = lc
