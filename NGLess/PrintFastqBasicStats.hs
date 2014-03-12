{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module PrintFastqBasicStats
    (
        calculateEncoding,
        sanger_encoding_offset,
        illumina_1_encoding_offset,
        illumina_1_3_encoding_offset,
        getGCPercent,
        getEncoding,
        Encoding(..)
    ) where

import Data.Char

data Encoding = Encoding {name :: String, offset :: Int} deriving(Show,Eq)

-- Constants
sanger_encoding_offset = 33
illumina_1_encoding_offset = 59
illumina_1_3_encoding_offset = 64



getGCPercent :: (Int,Int,Int,Int) -> Double
getGCPercent (bpA,bpC,bpG,bpT) =
    do
        let gcCount = fromIntegral (bpC + bpG)
            allBpCount = fromIntegral (bpA + bpC + bpG + bpT)
        ((gcCount / allBpCount) * 100) :: Double


getEncoding :: Char -> String
getEncoding lowC = name (calculateEncoding $ ord lowC)

--calculateEncoding :: Calculates the encoding by receiving the lowest quality character.
calculateEncoding :: Int -> Encoding
calculateEncoding lowC
        | lowC < sanger_encoding_offset  = error ("No known encodings with chars < 33 (Yours was "++ (show lowC) ++ ")")
        | lowC < illumina_1_encoding_offset =  Encoding "Sanger / Illumina 1.9" sanger_encoding_offset
        | lowC < illumina_1_3_encoding_offset = Encoding "Illumina <1.3" illumina_1_encoding_offset
        | lowC == (illumina_1_3_encoding_offset+1) = Encoding "Illumina 1.3" illumina_1_3_encoding_offset
        | lowC <=  126 = Encoding "Illumina 1.5" illumina_1_3_encoding_offset
        | otherwise = error ("No known encodings with chars > 126 (Yours was "++ (show lowC) ++")")
