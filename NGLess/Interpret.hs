{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import Language
import NumberOfChars
import Data.Text
import PrintFastqBasicStats


interpret (Sequence es) = handleSequence es
interpret (Assignment var func) = variableAssignment var >> interpretFunctions func 
variableAssignment (Variable varName) = print varName
interpretFunctions (FunctionCall functionType [ConstStr fname] exprs block ) = 
	case functionType of
		Ffastq -> readFastQ (unpack fname)
		_ -> print functionType -- all the other functionCalls


-- functions to handle interpretation
readFastQ fname =
	do
	 x <- readFile fname
	 printFileName fname
	 printGCPercent (countBps x)
	 printEncoding (lowestChar x)
	 printNumberSequences x
	 printSequenceSize x
	 


handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret e >> handleSequence es
handleSequence [] = return ()

