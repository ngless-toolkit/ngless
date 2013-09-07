{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import qualified Data.ByteString.Lazy.Char8 as B

import Language
import NumberOfChars
import Data.Text
import Data.Char
import PerBaseQualityScores
import PrintFastqBasicStats


interpret (Sequence es) = handleSequence es
interpret (Assignment var func) = variableAssignment var >> interpretFunctions func
variableAssignment (Variable varName) = print varName
interpretFunctions (FunctionCall functionType [ConstStr fname] exprs block ) =
    case functionType of
        Ffastq -> readFastQ (unpack fname)
        _ -> print functionType -- all the other functionCalls


-- functions to handle interpretation
readFastQ fname = do
    contents <- B.readFile fname
    let fileData = iterateFile contents
    printFileName fname
    print (lc fileData)
    printGCPercent (bpCounts fileData)
    printEncoding (lc fileData)
    printNumberSequences (nSeq fileData)
    printSequenceSize (seqSize fileData)
--  print (qualCounts fileData)
    print $ calculateMean (qualCounts fileData) (ord (lc fileData))



handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret e >> handleSequence es
handleSequence [] = return ()

