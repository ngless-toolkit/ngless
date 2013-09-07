{-# LANGUAGE BangPatterns #-}
module PrintFastqBasicStats
    (
        printFileName,
        printGCPercent,
        printNumberSequences,
        printSequenceSize,
        printEncoding,
        calculateEncoding,
        Encoding(..)
    ) where

import Data.Char
import NumberOfChars

data Encoding = Encoding {name :: String, offset :: Int}

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
printEncoding lc = putStrLn( "Encoding: " ++ (name (calculateEncoding $ ord lc))  )

calculateEncoding :: Int -> Encoding
calculateEncoding lc
        | lc < sanger_encoding_offset  = error ("No known encodings with chars < 33 (Yours was "++ (show lc) ++ ")")
        | lc < illumina_1_encoding_offset =  Encoding "Sanger / Illumina 1.9" sanger_encoding_offset
        | lc < illumina_1_3_encoding_offset = Encoding "Illumina <1.3" illumina_1_encoding_offset
        | lc == (illumina_1_3_encoding_offset+1) = Encoding "Illumina 1.3" illumina_1_3_encoding_offset
        | lc <=  126 = Encoding "Illumina 1.5" illumina_1_3_encoding_offset
        | otherwise = error ("No known encodings with chars > 126 (Yours was "++ (show lc) ++")")
