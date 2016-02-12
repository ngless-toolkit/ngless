{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , FQStatistics(..)
    , srLength
    , guessEncoding
    , encodingOffset
    , encodingName
    , readReadSet
    , parseFastQ
    , asFastQ
    , fqConduitR
    , gcFraction
    , statsFromFastQ
    , calculateStatistics
    , calcPercentile
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Control.DeepSeq (NFData(..))
import Data.Conduit         (($$), (=$=))
import Data.Conduit.Async   ((=$=&), ($$&))
import Control.Monad
import Control.Monad.Except

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VUM

import Data.Convertible (convert)
import Data.Char
import Data.Word


import NGLess.NGError
import Utils.Conduit
import Utils.Vector (zeroVec, unsafeIncrement)
import Utils.Utils

data ShortRead = ShortRead
        { srHeader :: !B.ByteString
        , srSequence :: !B.ByteString
        , srQualities :: !B.ByteString
        } deriving (Eq, Show, Ord)

data FastQEncoding = SangerEncoding | SolexaEncoding deriving (Eq, Bounded, Enum, Show, Ord)

data FQStatistics = FQStatistics
                { bpCounts :: (Int, Int, Int, Int)
                , lc :: Word8
                , qualCounts ::  [V.Vector Int]
                , nSeq :: Int
                , seqSize :: (Int,Int)
                } deriving(Eq,Show)

instance NFData FQStatistics where
    rnf (FQStatistics (!_,!_,!_,!_) !_ qv !_ (!_,!_)) = rnf qv

srLength = B.length . srSequence

encodingOffset :: Num a => FastQEncoding -> a
encodingOffset SangerEncoding = 33
encodingOffset SolexaEncoding = 64

encodingName :: FastQEncoding -> String
encodingName SangerEncoding = "Sanger (also recent Illumina)"
encodingName SolexaEncoding = "Solexa (older Illumina)"

guessEncoding :: (MonadError NGError m) => Word8 -> m FastQEncoding
guessEncoding lowC
    | lowC < 33 = throwDataError ("No known encodings with chars < 33 (Yours was "++ show lowC ++ ")")
    | lowC < 58 = return SangerEncoding
    | otherwise = return SolexaEncoding


parseFastQ :: FastQEncoding -> BL.ByteString -> [ShortRead]
parseFastQ enc contents = parse' . map BL.toStrict . BL.lines $ contents
    where
        parse' [] = []
        parse' (lid:lseq:_:lqs:xs) = (createRead enc lid lseq lqs) : parse' xs
        parse' xs = error ("Number of lines is not multiple of 4! EOF:" ++ show xs)

createRead enc rid rseq rqs = ShortRead rid rseq (decodeQual rqs)
    where
        decodeQual = B.map sub
        offset = encodingOffset enc
        sub v = v - offset

asFastQ :: FastQEncoding -> [ShortRead] -> BL.ByteString
asFastQ enc rs = BL.fromChunks (asFastQ' rs)
    where
        asFastQ' [] = []
        asFastQ' ((ShortRead a b c):rss) = [a, "\n", b, "\n+\n", encodeQual c, "\n"] ++ asFastQ' rss
        encodeQual = B.map ((+) (encodingOffset enc))

readReadSet :: FastQEncoding -> FilePath -> IO [ShortRead]
readReadSet enc fn = parseFastQ enc <$> readPossiblyCompressedFile fn

fqConduitR :: (Monad m, MonadError NGError m) => FastQEncoding -> C.Conduit ByteLine m ShortRead
fqConduitR enc = groupC 4 =$= CL.mapM parseShortReads
    where
        parseShortReads [ByteLine rid, ByteLine rseq, _, ByteLine rqs] = return (createRead enc rid rseq rqs)
        parseShortReads _ = throwDataError ("Number of lines in FastQ file is not multiple of 4! EOF found" :: String)


statsFromFastQ :: FilePath -> NGLessIO FQStatistics
statsFromFastQ fp =
    conduitPossiblyCompressedFile fp
        =$= linesC
        $$ fqStatsC

getEnc :: Maybe FastQEncoding -> FilePath -> NGLessIO FastQEncoding
getEnc (Just e) _ = return e
getEnc Nothing fp = do
    let countMin :: (Int, Word8) -> Word8 -> (Int, Word8)
        countMin (!c,!m) m' = (c+1, min m m')
        minLc :: (MonadError NGError m) => [ByteLine] -> m Word8
        minLc [_,_,_,qs] = return . B.minimum . unwrapByteLine $ qs
        minLc _ = throwDataError ("Malformed FASTQ file: '" ++ fp ++ "': number of lines is not a multiple of 4")

    (c,m) <- conduitPossiblyCompressedFile fp
        =$= linesC
        =$= groupC 4
        =$= CL.isolate 100
        =$= CL.mapM minLc
        $$ CL.fold countMin (0,maxBound :: Word8)
    when (c < 2) $
        throwDataError ("Cannot determine encoding for input file '" ++ fp ++ "'. File is too short [ngless requires at least 2 sequences]")
    liftIO $ putStrLn ("Found min char: " ++ show m ++ " in " ++ show c ++ " sequences")
    guessEncoding m


data StatsIter = StatsIter !Int !Int !Int !Int [VUM.IOVector Int] (VUM.IOVector Int)

fqStatsC :: C.Sink ByteLine NGLessIO FQStatistics
fqStatsC = do
        charCounts <- liftIO $ zeroVec 256
        r <- groupC 4
            =$= CL.mapM getP
            =$= CL.foldM update (StatsIter 0 256 (maxBound :: Int) 0 [] charCounts)
        liftIO (freeze r)
    where
        getP [_,s,_,q] = return (s,q)
        getP  _ = throwDataError ("Malformed FASTQ file: number of lines is not a multiple of 4" :: String)

        freeze :: StatsIter -> IO FQStatistics
        freeze (StatsIter n lcT minSeq maxSeq qcs charCounts) = do
                qcs' <- mapM V.freeze qcs
                aCount <- getNoCaseV charCounts 'a'
                cCount <- getNoCaseV charCounts 'c'
                gCount <- getNoCaseV charCounts 'g'
                tCount <- getNoCaseV charCounts 't'
                return (FQStatistics (aCount, cCount, gCount, tCount) (fromIntegral lcT) qcs' n (minSeq, maxSeq))
        getNoCaseV c p = do
            lower <- VUM.read c (ord p)
            upper <- VUM.read c (ord . toUpper $ p)
            return (lower + upper)


gcFraction :: FQStatistics -> Double
gcFraction res = gcCount / allBpCount
    where
        (bpA,bpC,bpG,bpT) = bpCounts res
        gcCount = fromIntegral $ bpC + bpG
        allBpCount = fromIntegral $ bpA + bpC + bpG + bpT

update :: StatsIter -> (ByteLine, ByteLine) -> NGLessIO StatsIter
update (StatsIter n lcT minSeq maxSeq qcs charCounts) (ByteLine bps,ByteLine qs) = liftIO $ do
    forM_ [0 .. B.length bps - 1] $ \i -> do
        let bi = B.index bps i
        unsafeIncrement charCounts (convert bi)
    let len = B.length bps
        qsM = convert . B.minimum $ qs
    nq <- replicateM (len - maxSeq) $
        zeroVec 256
    let qcs' = qcs ++ nq
    forM_ (zip [0 .. B.length qs - 1] qcs') $ \(i,qv) -> do
        let qi = convert (B.index qs i)
        unsafeIncrement qv qi
    return (StatsIter (n + 1) (min qsM lcT) (min minSeq len) (max maxSeq len) qcs' charCounts)

-- accUntilLim :: given lim, each position of the array is added until lim.
-- Is returned the elem of the array in that position.
accUntilLim :: V.Vector Int -> Int -> Int
accUntilLim bps lim = case V.findIndex (>= lim) $ V.postscanl (+) 0 bps of
      Just v -> v
      Nothing -> error ("ERROR: Must exist a index with a accumulated value larger than " ++ show lim)


calculateStatistics :: FQStatistics -> FastQEncoding -> [(Int, Int, Int, Int)]
calculateStatistics FQStatistics{qualCounts=qCounts} enc = Prelude.map statistics qCounts
    where
        encOffset = encodingOffset enc
        statistics :: V.Vector Int -> (Int, Int, Int, Int)
        statistics bps = (bpSum `div` elemTotal
                                , calcPercentile' 0.50
                                , calcPercentile' 0.25
                                , calcPercentile' 0.75)
            where bpSum = calcBPSum bps encOffset
                  elemTotal = V.sum bps
                  calcPercentile' :: Double -> Int
                  calcPercentile' p = calcPercentile bps elemTotal p - encOffset

-- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2.
-- 'a' and 'b' minus encoding.
calcBPSum :: V.Vector Int -> Int -> Int
calcBPSum qs offset = V.ifoldl' (\n i q -> (n + (i - offset) * q)) 0 qs

calcPercentile :: V.Vector Int -> Int -> Double -> Int
calcPercentile bps elemTotal perc = accUntilLim bps val'
    where val' = ceiling (fromIntegral elemTotal * perc)

