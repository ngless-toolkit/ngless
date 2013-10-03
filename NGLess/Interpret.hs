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
interpret' (BinaryOp opType lvalue rvalue)  = putStrLn . show $ interpretBinaryOperators opType lvalue rvalue
interpret' e = error (Prelude.concat ["interpret: cannot handle ", show e])

variableAssignment (Variable varName) = print varName

-- Handling Function calls

interpretFunctions (FunctionCall Ffastq (ConstStr fname) _exprs _block) = readFastQ (T.unpack fname)
interpretFunctions (FunctionCall Fpreprocess expr _exprs (Just _block)) = do
    fileReads <- (B.readFile "../Ngless-Mine/test.fq" )  -- Simulate the read of a expr...
    let fastq = createReadSet fileReads
    writeFile "/tmp/test_.fq" (show (fpreprocess fastq _block))
interpretFunctions _ = error "interpretFunctions does not handle non-FunctionCall expressions"

-- handling PreProcess -- 

handlePreProcessSequence :: [Expression] -> FPreProcess.Read -> Maybe FPreProcess.Read
handlePreProcessSequence (e:es) eachRead = 
    case e of 
      (Condition expr seq1 seq2) -> interpretConditions expr seq1 seq2 eachRead >>= handlePreProcessSequence es
      _ -> interpretPreProcessFunctions' e eachRead >>= handlePreProcessSequence es
handlePreProcessSequence [] eachRead = return (eachRead)


interpretPreProcessFunctions' :: Expression -> FPreProcess.Read -> Maybe FPreProcess.Read
interpretPreProcessFunctions' (Sequence es) eachRead = handlePreProcessSequence es eachRead
interpretPreProcessFunctions' (Assignment var func) eachRead = interpretPreProcessFunctions' func eachRead
interpretPreProcessFunctions' (FunctionCall Fsubstrim _ expr _) eachRead = Just $ substrim (getvalue expr) eachRead
interpretPreProcessFunctions' (Discard) eachRead = Nothing
interpretPreProcessFunctions' (IndexExpression var index) eachRead = interpretIndexExpr var index eachRead
interpretPreProcessFunctions' e _ = error (Prelude.concat ["interpretPreProcessFunctions: cannot handle ", show e])


interpretConditions :: Expression -> Expression -> Expression -> FPreProcess.Read -> Maybe FPreProcess.Read
interpretConditions (BinaryOp opType lvalue rvalue) seq1 seq2 eachRead = do
  case interpretBinaryOperators opType lvalue rvalue of 
    Left res -> if  (res) then interpretPreProcessFunctions' seq1 eachRead >>= interpretPreProcessFunctions' seq2 else Just eachRead
    Right res -> Just eachRead 
interpretConditions e _ _ _ = error (Prelude.concat ["interpretConditions: cannot handle ", show e])

---- data BOp = BOpAdd | BOpMul | BOpGT | BOpGTE | BOpLT | BOpLTE | BOpEQ | BOpNEQ
interpretBinaryOperators :: BOp -> Expression -> Expression -> Either Bool Integer
interpretBinaryOperators BOpLT lexpr rexpr = Left $ interpretUnaryOperators lexpr < interpretUnaryOperators rexpr 
interpretBinaryOperators BOpGT lexpr rexpr = Left $ interpretUnaryOperators lexpr > interpretUnaryOperators rexpr 
interpretBinaryOperators BOpLTE lexpr rexpr = Left $ interpretUnaryOperators lexpr <= interpretUnaryOperators rexpr
interpretBinaryOperators BOpGTE lexpr rexpr = Left $ interpretUnaryOperators lexpr >= interpretUnaryOperators rexpr
interpretBinaryOperators BOpEQ lexpr rexpr = Left $ interpretUnaryOperators lexpr == interpretUnaryOperators rexpr
interpretBinaryOperators BOpNEQ lexpr rexpr = Left $ interpretUnaryOperators lexpr /= interpretUnaryOperators rexpr
interpretBinaryOperators BOpAdd lexpr rexpr = Right $ interpretUnaryOperators lexpr + interpretUnaryOperators rexpr
interpretBinaryOperators BOpMul lexpr rexpr = Right $ interpretUnaryOperators lexpr * interpretUnaryOperators rexpr


interpretUnaryOperators :: Expression -> Integer
interpretUnaryOperators (UnaryOp UOpLen var) = 10 --TODO: length var, after lookup implemented 
interpretUnaryOperators (ConstNum num) = num
interpretUnaryOperators e = error (Prelude.concat ["interpretUnaryOperators: cannot handle ", show e])


-- INDEX
-- Should be a lookup on var! For now will receive a read as argument
-- handle read [5:10], read[:5], read[5:], read[:] 
interpretIndexExpr :: Expression -> Index -> FPreProcess.Read -> Maybe FPreProcess.Read

--Case : array[x] -- TODO: Makes sense to have a read of 1 char
--interpretIndexExpr var (IndexOne expr) eachRead = 
--  let value' = (fromInteger $ interpretUnaryOperators expr)
--  in Just $ FPreProcess.Read  (FPreProcess.readId eachRead)
--                              (FPreProcess.seq eachRead !! value')    
--                              (FPreProcess.qual eachRead !! value')     

-- Case : array[:x]
interpretIndexExpr var (IndexTwo Nothing (Just end)) eachRead = 
  let value' = (fromInteger $ interpretUnaryOperators end)
  in Just $ FPreProcess.Read  (FPreProcess.readId eachRead)
                              (Prelude.take value' $ FPreProcess.seq eachRead)    
                              (Prelude.take value' $ FPreProcess.qual eachRead)

-- Case : array[x:]
interpretIndexExpr var (IndexTwo (Just start) Nothing) eachRead =   
  let value' = (fromInteger $ interpretUnaryOperators start)
  in Just $ FPreProcess.Read  (FPreProcess.readId eachRead)
                              (Prelude.drop value' $ FPreProcess.seq eachRead)    
                              (Prelude.drop value' $ FPreProcess.qual eachRead)
-- Case : array[y:x]
interpretIndexExpr var (IndexTwo (Just start) (Just end)) eachRead = 
  let startIndex' = (fromInteger $ interpretUnaryOperators start)
      endIndex' = (fromInteger $ interpretUnaryOperators end)
      calcIndex' = (endIndex' - startIndex') -- since the start position is removed.
  in Just $ FPreProcess.Read  (FPreProcess.readId eachRead)
                              (Prelude.take calcIndex' . Prelude.drop startIndex' $ FPreProcess.seq eachRead)    
                              (Prelude.take calcIndex' . Prelude.drop startIndex' $ FPreProcess.qual eachRead)  
interpretIndexExpr e _ _ = error (Prelude.concat ["interpretIndexExpr: cannot handle ", show e])

fpreprocess :: [FPreProcess.Read] -> Block -> [FPreProcess.Read]
fpreprocess readSet _block =
    let   map' = mapMaybe (interpretPreprocessBlock _block) readSet
          res = map' `using` parList rseq
    in res

interpretPreprocessBlock :: Block -> FPreProcess.Read -> Maybe FPreProcess.Read
interpretPreprocessBlock (Block var expr) = interpretPreProcessFunctions' expr


handleSequence :: [Expression] -> IO ()
handleSequence (e:es) = interpret' e >> handleSequence es
handleSequence [] = return ()


-- Aux functions to handle interpretation

createReadSet :: B.ByteString -> [FPreProcess.Read]
createReadSet fileReads = createReadSet' (Prelude.map (B.unpack) (B.lines fileReads)) []
    where createReadSet' (rid:readSeq:_:readQual:xs) res =  createReadSet' xs ((FPreProcess.Read rid readSeq readQual) : res)
          createReadSet' [] res = res
          createReadSet' _ _ = error "Number of lines is not multiple of 4!"

getvalue :: [(Variable, Expression)] -> Int
getvalue [((Variable v),(ConstNum value))] = assert (v == (T.pack "min_quality")) fromIntegral value
getvalue _ = error "unexpected"
--
