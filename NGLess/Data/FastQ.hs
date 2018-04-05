{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf #-}
{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , FastQFilePath(..)
    , FQStatistics(..)
    , nBasepairs
    , srSlice
    , srLength
    , encodingOffset
    , encodingName
    , fqDecodeVector
    , fqDecodeC
    , fqEncode
    , fqEncodeC
    , gcFraction
    , nonATCGFrac
    , statsFromFastQ
    , fqStatsC
    , qualityPercentiles
    , interleaveFQs
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Control.DeepSeq (NFData(..))
import           Data.Conduit ((.|))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Exception

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed.Mutable as VUM

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import Foreign.C.Types
import Foreign.C.String

import Data.IORef
import Data.Maybe
import Data.Int
import Data.Char
import Data.Word

import NGLess.NGError
import Utils.Conduit
import Utils.Vector (unsafeIncrement)

foreign import ccall "updateCharCount" c_updateCharCount :: CUInt -> CString -> Ptr Int -> IO ()

    -- | Represents a short read
data ShortRead = ShortRead
        { srHeader :: !B.ByteString
        , srSequence :: !B.ByteString
        , srQualities :: !(VS.Vector Int8) -- ^ these have been decoded
        } deriving (Eq, Show, Ord)

instance NFData ShortRead where
    rnf ShortRead{} = ()

data FastQEncoding = SangerEncoding | SolexaEncoding deriving (Eq, Bounded, Enum, Show, Ord)

data FastQFilePath = FastQFilePath
                        { fqpathEncoding :: !FastQEncoding -- ^ encoding
                        , fqpathFilePath :: FilePath -- ^ file_on_disk
                        } deriving (Eq, Show, Ord)

data FQStatistics = FQStatistics
                { bpCounts :: (Int, Int, Int, Int, Int) -- ^ number of (A, C, T, G, OTHER)
                , lc :: !Int8 -- ^ lowest quality value
                , qualCounts ::  [VU.Vector Int] -- ^ quality counts by position
                , nSeq :: !Int -- ^ number of sequences
                , seqSize :: (Int,Int) -- ^ min and max
                } deriving(Eq,Show)

instance NFData FQStatistics where
    rnf (FQStatistics (!_,!_,!_,!_,!_) !_ qv !_ (!_,!_)) = rnf qv

-- | Total number of base pairs
-- Returns an Integer as it can be > 2³¹
nBasepairs :: FQStatistics -> Integer
nBasepairs fqstats = sum $ map (sum . map toInteger . VU.toList) $ qualCounts fqstats

minQualityValue :: Num a => a
minQualityValue = -5

srLength = B.length . srSequence

srSlice :: Int -> Int -> ShortRead -> ShortRead
srSlice s n (ShortRead rId rS rQ) = assert (B.length rS >= s + n) $ ShortRead rId (B.take n $ B.drop s rS) (VS.slice s n rQ)

encodingOffset :: Num a => FastQEncoding -> a
encodingOffset SangerEncoding = 33
encodingOffset SolexaEncoding = 64

encodingName :: FastQEncoding -> String
encodingName SangerEncoding = "Sanger (33 offset)"
encodingName SolexaEncoding = "Solexa (64 offset)"


-- | encode ShortRead as a ByteString (FastQ format)
fqEncodeC :: (Monad m) => FastQEncoding -> C.Conduit ShortRead m B.ByteString
fqEncodeC enc = CL.map (fqEncode enc)

-- Using B.map instead of this function makes this loop be one of the functions
-- with the highest memory allocation in ngless.
bsAdd :: VS.Vector Int8 -> Int8 -> B.ByteString
bsAdd c delta = BI.unsafeCreate cn $ \p -> copyAddLoop p 0
    where
        cn = VS.length c
        copyAddLoop p i
            | i == cn = return ()
            | otherwise = do
                poke (p `plusPtr` i) ((fromIntegral $ c VS.! i + delta) :: Word8)
                copyAddLoop p (i + 1)

vSub :: B.ByteString -> Int8 -> VS.Vector Int8
vSub qs delta = VS.generate (B.length qs) $ \i -> fromIntegral (B.index qs i) - delta

fqEncode :: FastQEncoding -> ShortRead -> B.ByteString
fqEncode enc (ShortRead a b c) = B.concat [a, "\n", b, "\n+\n", bsAdd c offset, "\n"]
    where
        offset :: Int8
        offset = encodingOffset enc


-- | Decode a FastQ file as a Conduit
--
-- Throws DataError if the stream is not in valid FastQ format
fqDecodeC :: (MonadError NGError m) => FastQEncoding -> C.Conduit ByteLine m ShortRead
fqDecodeC enc = C.awaitForever $ \(ByteLine rid) ->
        lineOrError4 $ \rseq ->
            lineOrError4 $ \_ ->
                lineOrError4 $ \rqs->
                    if B.length rseq == B.length rqs
                        then C.yield $! ShortRead rid rseq (vSub rqs offset)
                        else throwDataError "Length of quality line is not the same as sequence"
    where
        offset :: Int8
        offset = encodingOffset enc
        lineOrError4 f = C.await >>=
                            maybe
                                (throwDataError "Number of lines in FastQ file is not multiple of 4! EOF found")
                                (f . unwrapByteLine)


-- | Decode a vector of ByteLines into a vector of ShortReads.
fqDecodeVector :: FastQEncoding -> V.Vector ByteLine -> NGLess (V.Vector ShortRead)
fqDecodeVector enc vs
        | V.length vs `mod` 4 /= 0 = throwDataError "Number of input lines in FastQ file is not a multiple of 4"
        | otherwise = return $! V.generate (V.length vs `div` 4) parse1
    where
        offset :: Int8
        offset = encodingOffset enc
        parse1 i = ShortRead rid rseq (vSub rqs offset)
            where
                rid  = unwrapByteLine $ vs V.! (i*4)
                rseq = unwrapByteLine $ vs V.! (i*4 + 1)
                rqs  = unwrapByteLine $ vs V.! (i*4 + 3)

statsFromFastQ :: (MonadIO m, MonadError NGError m, MonadBaseControl IO m, MonadThrow m) => FilePath -> FastQEncoding -> m FQStatistics
statsFromFastQ fp enc = C.runConduitRes $
        conduitPossiblyCompressedFile fp
            .| linesCBounded
            .| fqDecodeC enc
            .| fqStatsC


fqStatsC :: forall m. (MonadIO m) => C.Sink ShortRead m FQStatistics
fqStatsC = do
        -- This is pretty ugly code, but threading the state through a foldM
        -- was >2x slower. In any case, all the ugliness is well hidden.
        (charCounts,stats,qualVals) <- liftIO $ do
            charCounts <- VSM.replicate 256 (0 :: Int)
            -- stats is [ Nr-sequences minSequenceSize maxSequenceSize ]
            stats <- VUM.replicate 3 0
            VUM.write stats 1 maxBound
            qualVals <- newIORef =<< VSM.new 0
            return (charCounts, stats, qualVals)
        CL.mapM_ (update charCounts stats qualVals)
        liftIO $ do
            qcs <- readIORef qualVals
            n <- VUM.read stats 0
            minSeq <- VUM.read stats 1
            maxSeq <- VUM.read stats 2
            qcs' <- forM [0 .. maxSeq - 1] $ \i -> do
                v <- VUM.new 256
                let base = i * 256
                forM_ [0 .. 255] $ \j ->
                    VSM.read qcs (base + j) >>= VUM.write v j
                VU.unsafeFreeze v
            let lcT = if n > 0
                            then findMinQValue qcs'
                            else 0
            ccounts <- VS.unsafeFreeze charCounts
            let aCount = getNoCaseV ccounts 'a'
                cCount = getNoCaseV ccounts 'c'
                gCount = getNoCaseV ccounts 'g'
                tCount = getNoCaseV ccounts 't'
                oCount = VS.sum ccounts - aCount - cCount - gCount - tCount
            return $! FQStatistics (aCount, cCount, gCount, tCount, oCount) (fromIntegral lcT) qcs' n (minSeq, maxSeq)
    where

        update :: VSM.IOVector Int -> VUM.IOVector Int -> IORef (VSM.IOVector Int) -> ShortRead -> m ()
        update !charCounts !stats qcs (ShortRead _ bps qs) = liftIO $ do
            let len = B.length bps
                qlen = 256*len
            prevLen <- VSM.length <$> readIORef qcs
            when (qlen > prevLen) $ do
                pqcs <- readIORef qcs
                nqcs <- VSM.grow pqcs (qlen - prevLen)
                forM_ [prevLen .. qlen - 1] $ \i ->
                    VSM.write nqcs i 0
                writeIORef qcs nqcs
            qcs' <- readIORef qcs
            B.unsafeUseAsCString bps $ \bps' ->
                VSM.unsafeWith charCounts $ \charCounts' ->
                    c_updateCharCount (toEnum len) bps' charCounts'
            let updateQCounts :: Int -> Int -> IO ()
                updateQCounts n i
                    | i == n = return ()
                    | otherwise = do
                        let qi = 256*i - minQualityValue + fromIntegral (qs VS.! i)
                        VSM.unsafeModify qcs' (+ 1) qi
                        updateQCounts n (i+1)
            updateQCounts (toEnum len) 0
            unsafeIncrement stats 0
            VUM.unsafeModify stats (min len) 1
            VUM.unsafeModify stats (max len) 2
            return ()

        getNoCaseV c p = c VS.! ord p + c VS.! (ord . toUpper $ p)

        findMinQValue :: [VU.Vector Int] -> Int
        findMinQValue = (flip (-) minQualityValue) . minimum . map findMinQValue'
        findMinQValue' :: VU.Vector Int -> Int
        findMinQValue' qs = fromMaybe 256 (VU.findIndex (/= 0) qs)

interleaveFQs :: (Monad m, MonadError NGError m, MonadResource m, MonadBaseControl IO m) => [(FastQFilePath, FastQFilePath)] -> [FastQFilePath] -> C.Source m B.ByteString
interleaveFQs pairs singletons = do
            sequence_ [interleavePair f0 f1 | (FastQFilePath _ f0, FastQFilePath _ f1) <- pairs]
            sequence_ [conduitPossiblyCompressedFile f | FastQFilePath _ f <- singletons]
    where
        interleavePair :: (Monad m, MonadError NGError m, MonadResource m, MonadBaseControl IO m) => FilePath -> FilePath -> C.Source m B.ByteString
        interleavePair f0 f1 =
                ((conduitPossiblyCompressedFile f0 .| linesCBounded .| CL.chunksOf 4) `zipSources` (conduitPossiblyCompressedFile f1 .| linesCBounded .| CL.chunksOf 4))
                .| C.awaitForever (\(r0,r1) -> C.yield (ul r0) >> C.yield (ul r1))
        zipSources a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)
        ul = B8.unlines . map unwrapByteLine

gcFraction :: FQStatistics -> Double
gcFraction res = gcCount / allBpCount
    where
        (bpA,bpC,bpG,bpT,_) = bpCounts res
        gcCount = fromIntegral $ bpC + bpG
        allBpCount = fromIntegral $ bpA + bpC + bpG + bpT

nonATCGFrac :: FQStatistics -> Double
nonATCGFrac fq = fromIntegral nO / fromIntegral (nA + nC + nT + nG + nO)
    where
        (nA, nC, nT, nG, nO) = bpCounts fq


qualityPercentiles :: FQStatistics -> [(Int, Int, Int, Int)]
qualityPercentiles FQStatistics{qualCounts=qCounts} = Prelude.map statistics qCounts
    where
        statistics :: VU.Vector Int -> (Int, Int, Int, Int)
        statistics qs = (bpSum `div` elemTotal + minQualityValue
                                , calcPercentile 0.50 + minQualityValue
                                , calcPercentile 0.25 + minQualityValue
                                , calcPercentile 0.75 + minQualityValue)
            where
                -- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2.
                -- 'a' and 'b' minus encoding.
                bpSum = VU.ifoldl' (\n i q -> n + i * q) 0 qs
                elemTotal = VU.sum qs

                calcPercentile :: Double -> Int
                calcPercentile perc = accUntilLim val'
                    where
                        val' = ceiling (fromIntegral elemTotal * perc)
                        accUntilLim :: Int -> Int
                        accUntilLim lim = case VU.findIndex (>= lim) $ VU.postscanl (+) 0 qs of
                              Just v -> v
                              Nothing -> error "ERROR: Logical impossibility in calcPercentile function"




