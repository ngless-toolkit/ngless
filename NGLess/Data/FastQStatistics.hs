{-# LANGUAGE OverloadedStrings #-}

module Data.FastQStatistics
    ( Result(..)
    , gcFraction
    , statsFromFastQ
    , calculateStatistics
    , _calcPercentile
    ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.STRef
import Data.Char
import Data.Word

import Data.FastQ
import Utils.Vector (zeroVec, unsafeIncrement)

data Result =  Result
                { bpCounts :: (Int, Int, Int, Int)
                , lc :: Word8
                , qualCounts ::  [V.Vector Int]
                , nSeq :: Int
                , seqSize :: (Int,Int)
                } deriving(Eq,Show)

-- strict tuple
data P4 = P4 !Int !Int !Int !Int

statsFromFastQ :: BL.ByteString -> Result
statsFromFastQ = statsFromFastQ' . fastqParse

gcFraction :: Result -> Double
gcFraction res = gcCount / allBpCount
    where
        (bpA,bpC,bpG,bpT) = bpCounts res
        gcCount = fromIntegral $ bpC + bpG
        allBpCount = fromIntegral $ bpA + bpC + bpG + bpT

fastqParse :: BL.ByteString -> [(B.ByteString, B.ByteString)]
fastqParse = fastqParse' . BL.lines
    where
        fastqParse' [] = []
        fastqParse' (_:s:_:q:ls) = (toStrict s, toStrict q):fastqParse' ls
        fastqParse' _ = error "fastqParse (statistics module): malformed file (nr of lines not a multiple of 4)"
        toStrict = B.concat . BL.toChunks


statsFromFastQ' :: [(B.ByteString, B.ByteString)] -> Result
statsFromFastQ' seqs = runST $ do
    charCounts <- zeroVec 256
    qualCountsT <- newSTRef []
    P4 n lcT minSeq maxSeq <- foldM (update charCounts qualCountsT) (P4 0 256 (maxBound :: Int) 0) seqs
    qualCountsT' <- readSTRef qualCountsT >>= mapM V.freeze
    aCount <- getNoCaseV charCounts 'a'
    cCount <- getNoCaseV charCounts 'c'
    gCount <- getNoCaseV charCounts 'g'
    tCount <- getNoCaseV charCounts 't'
    return (Result (aCount, cCount, gCount, tCount) (fromIntegral lcT) qualCountsT' n (minSeq, maxSeq))

update charCounts qualCountsT (P4 n lcT minSeq maxSeq) (bps,qs) = do
    forM_ [0 .. B.length bps - 1] $ \i -> do
        let bi = ord (B.index bps i)
        unsafeIncrement charCounts bi
    let len = B.length bps
        qsM = ord . B.minimum $ qs
    replicateM_ (len - maxSeq) $ do
        nv <- zeroVec 256
        modifySTRef' qualCountsT (++[nv])
    qualCountsT' <- readSTRef qualCountsT
    forM_ (zip [0 .. B.length qs - 1] qualCountsT') $ \(i,qv) -> do
        let qi = ord (B.index qs i)
        unsafeIncrement qv qi
    return $! P4 (n + 1) (min qsM lcT) (min minSeq len) (max maxSeq len)

getNoCaseV c p = do
    lower <- VM.read c (ord p)
    upper <- VM.read c (ord . toUpper $ p)
    return (lower + upper)


-- accUntilLim :: given lim, each position of the array is added until lim.
-- Is returned the elem of the array in that position.
accUntilLim :: V.Vector Int -> Int -> Int
accUntilLim bps lim = case V.findIndex (>= lim) $ V.postscanl (+) 0 bps of
      Just v -> v
      Nothing -> error ("ERROR: Must exist a index with a accumulated value larger than " ++ show lim)


calculateStatistics :: Result -> FastQEncoding -> [(Int, Int, Int, Int)]
calculateStatistics Result{qualCounts=qCounts} enc = Prelude.map statistics qCounts
    where
        encOffset = encodingOffset enc
        statistics :: V.Vector Int -> (Int, Int, Int, Int)
        statistics bps = (bpSum `div` elemTotal
                                , _calcPercentile' 0.50
                                , _calcPercentile' 0.25
                                , _calcPercentile' 0.75)
            where bpSum = calcBPSum bps encOffset
                  elemTotal = V.sum bps
                  _calcPercentile' :: Double -> Int
                  _calcPercentile' p = _calcPercentile bps elemTotal p - encOffset

-- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2.
-- 'a' and 'b' minus encoding.
calcBPSum :: V.Vector Int -> Int -> Int
calcBPSum qs offset = V.ifoldl' (\n i q -> (n + (i - offset) * q)) 0 qs

_calcPercentile :: V.Vector Int -> Int -> Double -> Int
_calcPercentile bps elemTotal perc = accUntilLim bps val'
    where val' = ceiling (fromIntegral elemTotal * perc)

