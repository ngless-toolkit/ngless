{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import qualified Data.ByteString.Lazy.Char8 as B

import Control.Exception.Base
import Control.Parallel.Strategies

import Language
import Data.Text as T
import Data.Maybe
import ProcessFastQ
import FPreProcess


interpret :: [(Int,Expression)] -> IO ()
interpret [] = return ()
interpret ((_,e):es) = interpret' e >> interpret es


interpret' :: Expression -> IO ()
interpret' (Sequence es) = handleSequence es
interpret' (Assignment var func) = variableAssignment var >> interpretFunctions func
interpret' func@(FunctionCall _ _ _ _ ) = interpretFunctions func
interpret' e = error (Prelude.concat ["interpret: cannot handle ", show e])

variableAssignment (Variable varName) = print varName

-- Handling Function calls

interpretFunctions (FunctionCall Ffastq (ConstStr fname) _exprs _block) = readFastQ (T.unpack fname)
interpretFunctions (FunctionCall Fpreprocess expr _exprs (Just _block)) = do
    fileReads <- (B.readFile "../Ngless-Mine/test.fq" )  -- Simulate the read of a expr...
    let fastq = createReadSet fileReads
    writeFile "/tmp/test_.fq" (show (fpreprocess fastq _block))
interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"

-- handling PreProcess -- TODO: Refactor Code.

handlePreProcessSequence :: [Expression] -> FPreProcess.Read -> Maybe FPreProcess.Read
handlePreProcessSequence (e:es) eachRead = interpretPreProcessFunctions' e eachRead >>= handlePreProcessSequence es
handlePreProcessSequence [] eachRead = return (eachRead)

interpretPreProcessFunctions' :: Expression -> FPreProcess.Read -> Maybe FPreProcess.Read
interpretPreProcessFunctions' (Sequence es) eachRead = handlePreProcessSequence es eachRead
interpretPreProcessFunctions' (Assignment var func) eachRead = interpretPreProcessFunctions' func eachRead
interpretPreProcessFunctions' (FunctionCall Fsubstrim _ expr _) eachRead = Just $ substrim (getvalue expr) eachRead
interpretPreProcessFunctions' e _ = error (Prelude.concat ["interpretPreProcessFunctions: cannot handle ", show e])

fpreprocess :: [FPreProcess.Read] -> Block -> [FPreProcess.Read]
fpreprocess readSet _block =
    let   map = mapMaybe (interpretPreprocessBlock _block) readSet
          res = map `using` parList rseq
    in res

interpretPreprocessBlock :: Block -> FPreProcess.Read -> Maybe FPreProcess.Read
interpretPreprocessBlock (Block var expr) = interpretPreProcessFunctions' expr


-- functions to handle interpretation
createReadSet :: B.ByteString -> [FPreProcess.Read]
createReadSet fileReads = createReadSet' (Prelude.map (B.unpack) (B.lines fileReads)) []
    where createReadSet' (readId:readSeq:_:readQual:xs) res =  createReadSet' xs ((FPreProcess.Read readId readSeq readQual) : res)
          createReadSet' [] res = res
          createReadSet' _ _ = error "Number of lines is not multiple of 4!"

getvalue :: [(Variable, Expression)] -> Int
getvalue [((Variable v),(ConstNum value))] = assert (v == (T.pack "min_quality")) fromIntegral value
getvalue _ = error "unexpected"
--

handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret' e >> handleSequence es
handleSequence [] = return ()
