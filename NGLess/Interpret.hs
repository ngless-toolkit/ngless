{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import Language
import NumberOfChars
import qualified Data.Text as T


interpret (Sequence es) = handleSequence es
interpret (Assignment var func) = variableAssignment var >> interpretFunctions func
interpret e = error (concat ["interpret: cannot handle ", show e])

variableAssignment (Variable varName) = print varName
interpretFunctions (FunctionCall functionType [ConstStr fname] _exprs _block) =
    case functionType of
        Ffastq -> readFastQ (T.unpack fname)
        _ -> print functionType -- all the other functionCalls
interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"


-- functions to handle interpretation
readFastQ fname =
    do
     x <- readFile fname
     print (countBps x)

handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret e >> handleSequence es
handleSequence [] = return ()

