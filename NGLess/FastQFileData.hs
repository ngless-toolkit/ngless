{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FastQFileData
    ( Result(..)
    , computeStats
    ) where

import Control.Monad
import Control.Monad.ST

import Control.Monad.Par

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.STRef
import Data.Char

data Result =  Result {bpCounts :: (Int, Int, Int, Int) , lc :: Char, qualCounts ::  [V.Vector Int], nSeq :: Int, seqSize :: (Int,Int)} deriving(Show)

-- strict tuple
data P3 = P3 !Int !Int !Int

computeStats :: BL.ByteString -> Result
computeStats = computeStats' . fastqParse
fastqParse :: BL.ByteString -> [(B.ByteString, B.ByteString)]
fastqParse = fastqParse' . BL.lines
    where
        fastqParse' [] = []
        fastqParse' (_:s:_:q:ls) = ((toStrict s, toStrict q):fastqParse' ls)
        fastqParse' _ = error "Malformed file (nr of lines not a multiple of 4)"
        toStrict = B.concat . BL.toChunks


computeStats' seqs = runST $ do
    charCounts <- zeroVec 256
    qualCountsT <- newSTRef []
    P3 n minSeq maxSeq <- foldM (update charCounts qualCountsT) (P3 0 (maxBound :: Int) (minBound :: Int)) seqs
    qualCountsT' <- readSTRef qualCountsT >>= mapM V.freeze
    aCount <- getV charCounts 'a'
    cCount <- getV charCounts 'c'
    gCount <- getV charCounts 'g'
    tCount <- getV charCounts 't'
    lcT <- getLC charCounts
    return (Result (aCount, cCount, gCount, tCount) lcT qualCountsT' n (minSeq, maxSeq))

update charCounts qualCountsT (P3 n minSeq maxSeq) (bps,qs) = do
    forM_ [0 .. B.length bps - 1] $ \i -> do
        let bi = ord (B.index bps i)
        incVec charCounts bi
    let len = B.length bps
    replicateM_ (len - maxSeq) $ do
        nv <- zeroVec 256
        modifySTRef' qualCountsT (++[nv])
    qualCountsT' <- readSTRef qualCountsT
    forM_ (zip [0 .. B.length qs - 1] qualCountsT') $ \(i,qv) -> do
        let qi = ord (B.index qs i)
        incVec qv qi
    return $! P3 (n + 1) (min minSeq len) (max maxSeq len)

incVec v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec 0
    return vec

getV c p = do
    lower <- VM.read c (ord p)
    upper <- VM.read c (ord . toUpper $ p)
    return (lower + upper)

getLC c = getLC' 0
    where
        n = VM.length c
        getLC' i | n == i = return (chr n)
        getLC' i = do
            ci <- VM.read c i
            if ci > 0
                then return (chr i)
                else getLC' (i + 1)
