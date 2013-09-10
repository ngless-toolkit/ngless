{-# LANGUAGE BangPatterns #-}
module PrintFastqBasicStats
    (
        printHtmlBasicStats,
        printHtmlEndScripts,
        calculateEncoding,
        Encoding(..)
    ) where

import Data.Char
import FastQFileData

data Encoding = Encoding {name :: String, offset :: Int}

-- Constants
sanger_encoding_offset = 33
illumina_1_encoding_offset = 59
illumina_1_3_encoding_offset = 64
--

printHtml fname = appendFile fname

printHtmlEndScripts destFile = printHtml destFile "\n<script src=\"d3.v3/d3.v3.js\"></script><script src=\"perbaseQualScoresData.js\"></script><script src=\"perBaseQualityScores.js\"></script></body></html>"

printHtmlBasicStats destDir fileData fname = do
							 let fileDest = (destDir ++ "/index.html")
							 printHtml fileDest ("<table border=\"1\">") 
							 printFileName fname fileDest 					  
							 printGCPercent (bpCounts fileData) fileDest
							 printEncoding (lc fileData) fileDest
							 printNumberSequences (nSeq fileData) fileDest
							 printSequenceSize (seqSize fileData) fileDest 
							 printHtml fileDest ("</table>")


printFileName fname fileDest = printHtml fileDest ("<tr><td> File Name is: </td> <td>" ++ fname ++ "</td></tr>")

printGCPercent :: (Int,Int,Int,Int) -> String -> IO ()
printGCPercent (bpA,bpC,bpG,bpT) fileDest =
    do
        let gcCount = fromIntegral (bpC + bpG)
            allBpCount = fromIntegral (bpA + bpC + bpG + bpT)
        printHtml fileDest ("<tr><td> %GC: </td> <td>" ++ (show ((gcCount / allBpCount) * 100)) ++ "</td></tr>" )

printNumberSequences nSeq fileDest =  printHtml fileDest ("<tr><td> Number of Sequences: </td> <td>" ++ (show nSeq) ++ "</td></tr>")
printSequenceSize seqSize fileDest =  printHtml fileDest ("<tr><td> Sequence length:  </td> <td>" ++ (show seqSize) ++ "</td></tr>")

printEncoding :: Char -> String -> IO ()
printEncoding lc fileDest =  printHtml fileDest ( "<tr><td> Encoding is: </td> <td> " ++ (name (calculateEncoding $ ord lc)) ++ "</td></tr>"  )

--calculateEncoding :: Calculates the encoding by receiving the lowest quality character.
calculateEncoding :: Int -> Encoding
calculateEncoding lc
        | lc < sanger_encoding_offset  = error ("No known encodings with chars < 33 (Yours was "++ (show lc) ++ ")")
        | lc < illumina_1_encoding_offset =  Encoding "Sanger / Illumina 1.9" sanger_encoding_offset
        | lc < illumina_1_3_encoding_offset = Encoding "Illumina <1.3" illumina_1_encoding_offset
        | lc == (illumina_1_3_encoding_offset+1) = Encoding "Illumina 1.3" illumina_1_3_encoding_offset
        | lc <=  126 = Encoding "Illumina 1.5" illumina_1_3_encoding_offset
        | otherwise = error ("No known encodings with chars > 126 (Yours was "++ (show lc) ++")")
