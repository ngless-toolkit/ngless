

module MapInterpretOperation
    (
    interpretMapOp
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Data.Vector.Unboxed as V

import Numeric
import InvokeExternalProgs
import SamBamOperations
import Language
import FileManagement

numDecimalPlaces :: Int
numDecimalPlaces = 2

interpretMapOp ref ds = do
    indexReference ref
    execMap' <- mapToReference ref (B.unpack ds)
    getSamStats execMap'
    return execMap'


getSamStats (NGOMappedReadSet fname) = do
    contents <- unCompress (T.unpack fname)
    let res' = samStats contents
        total' = getV res' (fromEnum Total)
        aligned' = getV res' (fromEnum Aligned)
        unique' = getV res' (fromEnum Unique)
    printNglessLn $ "Total reads: " ++ (show total')
    printNglessLn $ "Total reads aligned: " ++ (show aligned') ++ "[" ++ (showFloat' $ calcDiv aligned' total') ++ "%]"
    printNglessLn $ "Total reads Unique map: " ++ (show unique') ++ "[" ++ (showFloat' $ calcDiv unique' aligned') ++ "%]"
    printNglessLn $ "Total reads Non-Unique map: " ++ (show $ aligned' - unique') ++ "[" ++ (showFloat' $ 100 - (calcDiv unique' aligned')) ++ "%]"


getSamStats err = error $ "Type must be NGOMappedReadSet, but is: " ++ (show err)

getV vec i =  V.unsafeIndex vec i

calcDiv :: Int -> Int -> Double
calcDiv a b = 
      let x = fromIntegral a
          y = fromIntegral b
      in (x / y) * (100 :: Double) 

showFloat' num = showFFloat (Just numDecimalPlaces) num ""
