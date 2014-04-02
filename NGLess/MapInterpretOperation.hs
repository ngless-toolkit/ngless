

module MapInterpretOperation
    (
    interpretMapOp
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T


import InvokeExternalProgs
import SamBamOperations
import Language
import FileManagement


interpretMapOp ref ds = do
    indexReference ref
    execMap' <- mapToReference ref (B.unpack ds)
    getSamStats execMap'
    return execMap'


getSamStats (NGOMappedReadSet fname) = do
    contents <- unCompress (T.unpack fname)
    let res = length $ filter isAligned (readAlignments contents) 
    printNglessLn (show res)

getSamStats err = error $ "Type must be NGOMappedReadSet, but is: " ++ (show err)
