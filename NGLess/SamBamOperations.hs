{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    (
 SamLine(..),
 SamResult(..),
 isAligned,
 readAlignments,
 samStats
    ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Bits (testBit)
import Control.DeepSeq


data SamLine = SamLine
            { samQName :: S.ByteString
            , samFlag :: !Int
            , samRName :: S.ByteString
            , samPos :: !Int
            , samMapq :: !Int
            , samCigar :: S.ByteString
            , samRNext :: S.ByteString
            , samPNext :: !Int
            , samTLen :: !Int
            , samSeq :: S.ByteString
            , samQual :: S.ByteString
            } deriving (Eq, Show)


instance NFData SamLine where
    rnf (SamLine qn f r p m c rn pn tl s qual) = qn `seq` f `seq` r `seq` p `seq` m `seq` c `seq` rn `seq` pn `seq` tl `seq` s `seq` qual `seq` ()


data SamResult = Total | Aligned | Unique deriving (Enum)

isAligned :: SamLine -> Bool
isAligned = not . (`testBit` 2) . samFlag

isUnique :: SamLine -> Bool
isUnique =  (> 0) . samMapq

samStats = computeStats . readAlignments

computeStats sams = runST $ do
    initVec <- zeroVec 3
    forM_ sams $ \x -> do
        update initVec x
    V.freeze initVec

update result samLine = do
    incV True result (fromEnum Total)
    incV (isAligned samLine) result (fromEnum Aligned)
    incV (isUnique samLine) result (fromEnum Unique) 

readAlignments :: L.ByteString -> [SamLine]
readAlignments = readAlignments' . L8.lines

readAlignments' :: [L.ByteString] -> [SamLine]
readAlignments' [] = []
readAlignments' (l:ls)
    | L8.head l == '@' = readAlignments' ls
    | otherwise = (readSamLine l:readAlignments' ls)


readInt b = case L8.readInt b of
    Just (v,_) -> v
    _ -> error $ concat ["Expected int, got ", show b]

readSamLine :: L.ByteString -> SamLine
readSamLine line = case L8.split '\t' line of
    (tk0:tk1:tk2:tk3:tk4:tk5:tk6:tk7:tk8:tk9:tk10:_) -> SamLine
                (strict tk0)
                (readInt tk1)
                (strict tk2)
                (readInt tk3)
                (readInt tk4)
                (strict tk5)
                (strict tk6)
                (readInt tk7)
                (readInt tk8)
                (strict tk9)
                (strict tk10)
    tokens -> error $ concat ["Expected 11 tokens, only got ", show $ length tokens,"\n\t\tLine was '", show line, "'"]

strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec

incV False _ _ = return ()
incV True v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)
    return ()
  