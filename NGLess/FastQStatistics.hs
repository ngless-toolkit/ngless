{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FastQStatistics
    ( Result(..)
    , gcFraction
    , computeStats
    , printHtmlStatisticsData
    , _calcPercentile
    , _calculateStatistics
    , percentile50
    , lowerQuartile
    , upperQuartile
    , _createDataString
    ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed.Mutable as VM

import System.FilePath.Posix

import Data.STRef
import Data.Char
import VectorOperations(zeroVec, unsafeIncrement)

import PrintFastqBasicStats

data Result =  Result
                { bpCounts :: (Int, Int, Int, Int)
                , lc :: Char
                , qualCounts ::  [V.Vector Int]
                , nSeq :: Int
                , seqSize :: (Int,Int)
                } deriving(Eq,Show)

-- strict tuple
data P4 = P4 !Int !Int !Int !Int

computeStats :: BL.ByteString -> Result
computeStats = computeStats' . fastqParse

gcFraction :: Result -> Double
gcFraction res = (gcCount / allBpCount)
    where
        (bpA,bpC,bpG,bpT) = bpCounts res
        gcCount = fromIntegral $ bpC + bpG
        allBpCount = fromIntegral $ bpA + bpC + bpG + bpT

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
    P4 n lcT minSeq maxSeq <- foldM (update charCounts qualCountsT) (P4 0 256 (maxBound :: Int) 0) seqs
    qualCountsT' <- readSTRef qualCountsT >>= mapM V.freeze
    aCount <- getV charCounts 'a'
    cCount <- getV charCounts 'c'
    gCount <- getV charCounts 'g'
    tCount <- getV charCounts 't'
    return (Result (aCount, cCount, gCount, tCount) (chr lcT) qualCountsT' n (minSeq, maxSeq))

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


getV c p = do
    lower <- VM.read c (ord p)
    upper <- VM.read c (ord . toUpper $ p)
    return (lower + upper)


--constants
percentile50 = 0.5 :: Double
lowerQuartile = 0.25 :: Double
upperQuartile = 0.75 :: Double
--

-- accUntilLim :: given lim, each position of the array is added until lim.
-- Is returned the elem of the array in that position.
accUntilLim :: V.Vector Int -> Int -> Int
accUntilLim bps lim = do
    let i = V.findIndex (>= lim) $ V.postscanl (+) 0 bps
    case i of
      Just v -> v
      Nothing -> error ("ERROR: Must exist a index with a accumulated value smaller than " ++ (show i))


_createDataString :: [(Int, Int, Int, Int)] -> String
_createDataString stats = createDataString' stats "data = [\n" (1 :: Int)
    where createDataString' [] content _ = (content ++ "]\n")
          createDataString' (eachBp:xs) content bp = createDataString' xs (concatData eachBp content bp) (bp + 1)
          concatData (mean, median, lq, uq) content bp =
            content ++ "{ \"bp\" :" ++ show bp ++
            ", \"mean\" :" ++ show mean ++
            ", \"median\" :" ++ show median ++
            ", \"Lower Quartile\" :" ++ show lq ++
            ", \"Upper Quartile\" :" ++ show uq ++
             "},\n"


printHtmlStatisticsData qCounts minChar destDir = writeFile (destDir </> dataFileName) (_createDataString statisticsData')
    where
        statisticsData' = _calculateStatistics qCounts minChar
        dataFileName = "perbaseQualScoresData.js"

_calculateStatistics qCounts minChar = Prelude.map (statistics encOffset') qCounts
    where encOffset' = offset (calculateEncoding minChar)

--statistics :: Calculates the Quality Statistics of a given FastQ.
statistics :: Int -> V.Vector Int -> (Int, Int, Int, Int)
statistics encOffset bps = (bpSum `div` elemTotal
                            , _calcPercentile' percentile50
                            , _calcPercentile' lowerQuartile
                            , _calcPercentile' upperQuartile)
        where bpSum = calcBPSum bps encOffset
              elemTotal = V.sum bps
              _calcPercentile' p = (_calcPercentile bps elemTotal p) - encOffset

-- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2.
-- 'a' and 'b' minus encoding.
calcBPSum qc es = runST $ do
              n <- newSTRef 0
              let s = V.length qc
              forM_ [0..s - 1] $ \i -> do
                  modifySTRef n ((+) $ (i - es) * (V.unsafeIndex qc i))
              readSTRef n

--_calcPercentile :: Given a specific percentil,  calculates it's results.
_calcPercentile bps elemTotal perc = accUntilLim bps val'
    where val' = (ceiling (fromIntegral elemTotal * perc) :: Int)

