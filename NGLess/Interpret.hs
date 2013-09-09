{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.GZip as GZip

import Language
import FastQFileData
import Data.Text as T
import Data.Char
import PerBaseQualityScores
import PrintFastqBasicStats


interpret :: [(Int,Expression)] -> IO ()
interpret [] = return ()
interpret ((_,e):es) = interpret' e >> interpret es

interpret' :: Expression -> IO ()
interpret' (Sequence es) = handleSequence es
interpret' (Assignment var func) = variableAssignment var >> interpretFunctions func
interpret' e = error (Prelude.concat ["interpret: cannot handle ", show e])

variableAssignment (Variable varName) = print varName
interpretFunctions (FunctionCall functionType (ConstStr fname) _exprs _block) =
    case functionType of
        Ffastq -> readFastQ (T.unpack fname)
        _ -> print functionType -- all the other functionCalls
interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"


uncompress fname = 
			case isInfixOf (pack ".gz") (pack fname) of
				True -> fmap GZip.decompress (B.readFile fname)
				False -> B.readFile fname -- not compressed
				
-- functions to handle interpretation

readFastQ :: FilePath -> IO ()
readFastQ fname = do
    contents <- uncompress fname
    let fileData = iterateFile contents
    printFileName fname
    print (lc fileData)
    printGCPercent (bpCounts fileData)
    printEncoding (lc fileData)
    printNumberSequences (nSeq fileData)
    printSequenceSize (seqSize fileData)
--  print (qualCounts fileData)
    print $ calculateStatistics (qualCounts fileData) (ord (lc fileData))


handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret' e >> handleSequence es
handleSequence [] = return ()

