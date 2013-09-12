{-# LANGUAGE BangPatterns #-}
module PrintFastqBasicStats
    (
        printHtmlBasicStats,
        printHtmlEndScripts,
        calculateEncoding,
        dataFileName,
        Encoding(..)
    ) where

import Data.Char
import FastQFileData
import System.Directory

data Encoding = Encoding {name :: String, offset :: Int}

-- Constants
sanger_encoding_offset = 33
illumina_1_encoding_offset = 59
illumina_1_3_encoding_offset = 64
dataFileName = "perbaseQualScoresData.js"
bpQualStatsFileName = "/Html/perBaseQualityScores.js"
htmlFileName = "<tr><td> File Name is: </td> <td>"
htmlNumOfSeqs = "<tr><td> Number of Sequences: </td> <td>"
htmlSeqLen = "<tr><td> Sequence length:  </td> <td>"
htmlEncoding = "<tr><td> Encoding is: </td> <td> "
htmlGC = "<tr><td> %GC: </td> <td>"
htmlRowEnding =  "</td></tr>\n"
htmlScriptTag = "\"></script>\n<script src=\""
--

appendHtml fname = appendFile fname

-- TODO: Understand why cannot make an assignement inside a where or let.
printHtmlEndScripts destFile = do
        curDir <- getCurrentDirectory
        let dataPath' = dataFileName -- Each Dir has its own data
            bpStatisticsPlot' = curDir ++ bpQualStatsFileName

        appendHtml destFile ("\n" ++
            "<script src=\"http://d3js.org/d3.v3.min.js\" charset=\"utf-8\"></script>" ++
            "<script src=\"" ++ dataPath' ++ htmlScriptTag ++ bpStatisticsPlot' ++ "\"></script></body></html>")

printHtmlBasicStats destDir fileData fname = do
         let fileDest = (destDir ++ "/index.html")
         appendHtml fileDest ("<table class=\"table\" border=\"1\">")
         printFileName fname fileDest
         printGCPercent (bpCounts fileData) fileDest
         printEncoding (lc fileData) fileDest
         printNumberSequences (nSeq fileData) fileDest
         printSequenceSize (seqSize fileData) fileDest
         appendHtml fileDest ("</table>")


printFileName fname fileDest = appendHtml fileDest (htmlFileName ++ fname ++ htmlRowEnding)

printGCPercent :: (Int,Int,Int,Int) -> String -> IO ()
printGCPercent (bpA,bpC,bpG,bpT) fileDest =
    do
        let gcCount = fromIntegral (bpC + bpG)
            allBpCount = fromIntegral (bpA + bpC + bpG + bpT)
        appendHtml fileDest (htmlGC ++ (show ((gcCount / allBpCount) * 100)) ++ htmlRowEnding )

printNumberSequences nSeq fileDest =  appendHtml fileDest (htmlNumOfSeqs ++ (show nSeq) ++ htmlRowEnding)
printSequenceSize seqSize fileDest =  appendHtml fileDest (htmlSeqLen ++ (show seqSize) ++ htmlRowEnding)

printEncoding :: Char -> String -> IO ()
printEncoding lc fileDest =  appendHtml fileDest ( htmlEncoding ++ (name (calculateEncoding $ ord lc)) ++ htmlRowEnding  )

--calculateEncoding :: Calculates the encoding by receiving the lowest quality character.
calculateEncoding :: Int -> Encoding
calculateEncoding lc
        | lc < sanger_encoding_offset  = error ("No known encodings with chars < 33 (Yours was "++ (show lc) ++ ")")
        | lc < illumina_1_encoding_offset =  Encoding "Sanger / Illumina 1.9" sanger_encoding_offset
        | lc < illumina_1_3_encoding_offset = Encoding "Illumina <1.3" illumina_1_encoding_offset
        | lc == (illumina_1_3_encoding_offset+1) = Encoding "Illumina 1.3" illumina_1_3_encoding_offset
        | lc <=  126 = Encoding "Illumina 1.5" illumina_1_3_encoding_offset
        | otherwise = error ("No known encodings with chars > 126 (Yours was "++ (show lc) ++")")
