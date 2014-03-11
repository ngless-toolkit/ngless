{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module PrintFastqBasicStats
    (
        calculateEncoding,
        createBasicStatsJson,
        sanger_encoding_offset,
        illumina_1_encoding_offset,
        illumina_1_3_encoding_offset,
        Encoding(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Char

import FastQFileData
import JSONManager


data Encoding = Encoding {name :: String, offset :: Int} deriving(Show,Eq)

-- Constants
sanger_encoding_offset = 33
illumina_1_encoding_offset = 59
illumina_1_3_encoding_offset = 64


createBasicStatsJson filePath fileData fname = do
        let res = basicInfoToJson fname gc' enc' (nSeq fileData) (seqSize fileData) 
            resJS = BL.concat["var basicInfo = [", res, "];"]
        BL.writeFile filePath (resJS)
        where 
            gc' = (getGCPercent (bpCounts fileData))
            enc' = (getEncoding (lc fileData))

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
