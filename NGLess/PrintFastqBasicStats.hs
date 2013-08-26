{-# LANGUAGE BangPatterns #-}
module PrintFastqBasicStats
    ( 
	printFileName,
	printGCPercent,
	printNumberSequences,
	printSequenceSize,
	printEncoding
    ) where

import Data.Char

printFileName fname = putStrLn("File Name is: " ++ fname)

printGCPercent :: (Double,Double,Double,Double) -> IO ()
printGCPercent (bpA, bpC, bpG, bpT) = 
	do
	 let gcCount = (bpC + bpG)
	     allBpCount = (bpA + bpC + bpG + bpT)
	 putStrLn ("%GC " ++ (show (floor ((gcCount / allBpCount) * 100))))
	 
printNumberSequences content = putStrLn ("Number of Sequences: " ++ show ((fromIntegral (Prelude.length (Prelude.lines content))) / 4))
printSequenceSize content = putStrLn("Sequence length: " ++ (show (Prelude.length ((Prelude.lines content) !! 1))))


sanger_encoding_offset = 33
illumina_1_encoding_offset = 59
illumina_1_3_encoding_offset = 64

printEncoding :: Char -> IO ()
printEncoding lc = putStrLn( "Encoding: " ++ (calculateEncoding $ ord lc)  )

calculateEncoding :: Int -> String
calculateEncoding lc  
		| lc < sanger_encoding_offset  = "No known encodings with chars < 33 (Yours was "++ (show lc) ++ ")" 
		| lc < illumina_1_encoding_offset =  "Sanger / Illumina 1.9"
		| lc < illumina_1_3_encoding_offset = "Illumina <1.3"
		| lc == (illumina_1_3_encoding_offset+1) = "Illumina 1.3"
		| lc <=  126 = "Illumina 1.5"
		| otherwise = "No known encodings with chars > 126 (Yours was "++ (show lc) ++")"

