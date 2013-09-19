{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import Language
import Data.Text as T
import Data.Char
import Data.Maybe
import ProcessFastQ


interpret :: [(Int,Expression)] -> IO ()
interpret [] = return ()
interpret ((_,e):es) = interpret' e >> interpret es

interpret' :: Expression -> IO ()
interpret' (Sequence es) = handleSequence es
interpret' (Assignment var func) = variableAssignment var >> interpretFunctions func
interpret' func@(FunctionCall _ _ _ _ ) = interpretFunctions func 
interpret' e = error (Prelude.concat ["interpret: cannot handle ", show e])
--interpret' (FunctionCall Fpreprocess var _ (Just block)) = 


variableAssignment (Variable varName) = print varName

-- Handling Function calls
-- Structure -> FunctionCall FuncName Expression [(Variable, Expression)] (Maybe Block)

interpretFunctions (FunctionCall Ffastq (ConstStr fname) _exprs _block) = readFastQ (T.unpack fname)
interpretFunctions (FunctionCall Fpreprocess expr _exprs (Just _block)) = print _block
--do
--    fileReads <- (B.readFile "../Ngless-Mine/test.fq" )
--    writeFile "../Ngless-Mine/test_.fq" (fpreprocess fileReads block)

interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"

-- >

-- functions to handle interpretation

--fpreprocess fileReads block = do
--	let fileString = Prelude.map (B.unpack) (B.lines fileReads)
--	mapMaybe (interpretPreprocessBlock block) fileString-- file should come from the 'var'
       
       
--interpretPreprocessBlock :: Block -> String -> Maybe String
--interpretPreprocessBlock (Block var expr) str = interpret expr



handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret' e >> handleSequence es
handleSequence [] = return ()

