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
import NumberOfChars

printFileName fname = putStrLn("File Name is: " ++ fname)

printGCPercent :: (Int,Int,Int,Int) -> IO ()
printGCPercent (bpA,bpC,bpG,bpT) =
    do
        let gcCount = fromIntegral (bpC + bpG)
            allBpCount = fromIntegral (bpA + bpC + bpG + bpT)
        putStrLn ("%GC " ++ (show ((gcCount / allBpCount) * 100)))

printNumberSequences nSeq = putStrLn ("Number of Sequences: " ++ (show nSeq) )
printSequenceSize seqSize = putStrLn("Sequence length: " ++ (show seqSize))


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
