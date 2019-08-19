{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}
{- Copyright 2013-2019 NGLess Authors
 - License: MIT
 -}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , FastQFilePath(..)
    , ReadSet(..)
    , FQStatistics(..)
    , nBasepairs
    , srSlice
    , srLength
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
#ifdef IS_BUILDING_TEST
    , vSub
#endif
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Control.DeepSeq (NFData(..))
import           Data.Conduit ((.|))
import           Data.Monoid ((<>))
import           Control.Monad (forM_)
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Exception

import           System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline as C


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


C.context (C.baseCtx <> C.bsCtx <> C.vecCtx)
C.include "<stdint.h>"

foreign import ccall unsafe "updateCharCount" c_updateCharCount :: CUInt -> CString -> Ptr Int -> IO ()

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

data ReadSet = ReadSet
                { pairedSamples :: [(FastQFilePath, FastQFilePath)]
                , singleSamples :: [FastQFilePath]
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

-- short read length
srLength :: ShortRead -> Int
srLength = B.length . srSequence

-- slice a short read
srSlice ::
        Int -- ^ start pos
        -> Int -- ^ length of result
        -> ShortRead
        -> ShortRead
srSlice s n (ShortRead rId rS rQ) = assert (B.length rS >= s + n) $ ShortRead rId (B.take n $ B.drop s rS) (VS.slice s n rQ)

encodingOffset :: Num a => FastQEncoding -> a
encodingOffset SangerEncoding = 33
encodingOffset SolexaEncoding = 64

encodingName :: FastQEncoding -> String
encodingName SangerEncoding = "Sanger (33 offset)"
encodingName SolexaEncoding = "Solexa (64 offset)"


-- | encode ShortRead as a ByteString (FastQ format)
fqEncodeC :: (Monad m) => FastQEncoding -> C.ConduitT ShortRead B.ByteString m ()
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
vSub qs delta = unsafeDupablePerformIO $ do
    r <- VSM.new (B.length qs)
    [CU.block| void {
        int i;
        int len = $bs-len:qs;
        const char* in = $bs-ptr:qs;
        int8_t* out = $vec-ptr:(int8_t* r);
        for (i = 0; i < len; ++i) {
            out[i] = in[i] - $(int8_t delta);
        }
    }|]
    VS.unsafeFreeze r

fqEncode :: FastQEncoding -> ShortRead -> B.ByteString
fqEncode enc (ShortRead a b c) = B.concat [a, "\n", b, "\n+\n", bsAdd c offset, "\n"]
    where
        offset :: Int8
        offset = encodingOffset enc


-- | Decode a FastQ file as a Conduit
--
-- Throws DataError if the stream is not in valid FastQ format
fqDecodeC :: (MonadError NGError m) => FastQEncoding -> C.ConduitT ByteLine ShortRead m ()
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
fqDecodeVector :: Int -- ^ line number of first line (for error messages)
                -> FastQEncoding
                -> V.Vector ByteLine
                -> NGLess (V.Vector ShortRead)
fqDecodeVector lno enc vs
        | V.length vs `mod` 4 /= 0 = throwDataError $
                                        "Number of input lines in FastQ file is not a multiple of 4 (" ++ (show $ 1 + lno + V.length vs) ++ " lines)"
        | otherwise = runNGLess $! V.generateM (V.length vs `div` 4) parse1
    where
        offset :: Int8
        offset = encodingOffset enc
        parse1 i
                | B.length rseq == B.length rqs = return $ ShortRead rid rseq (vSub rqs offset)
                | otherwise = throwDataError $ "Length of quality line is not the same as sequence (line " ++ show (1 + i*4 + lno) ++ ")"
            where
                rid  = unwrapByteLine $ vs V.! (i*4)
                rseq = unwrapByteLine $ vs V.! (i*4 + 1)
                rqs  = unwrapByteLine $ vs V.! (i*4 + 3)

statsFromFastQ :: (MonadIO m, MonadError NGError m, MonadThrow m, MonadUnliftIO m) => FilePath -> FastQEncoding -> m FQStatistics
statsFromFastQ fp enc = C.runConduitRes $
        conduitPossiblyCompressedFile fp
            .| linesC
            .| fqDecodeC enc
            .| fqStatsC

fqStatsC :: forall m. (MonadIO m) => C.ConduitT ShortRead C.Void m FQStatistics
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
                    VSM.read qcs (base + j) >>= VUM.write v j . fromEnum
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

        update :: VSM.IOVector Int -> VUM.IOVector Int -> IORef (VSM.IOVector Int32) -> ShortRead -> m ()
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
            liftIO $ [CU.block| void {
                int len = $bs-len:bps;
                int minQ = $(int minQualityValue);
                int8_t* qs = $vec-ptr:(int8_t* qs);
                int32_t* qcs_ = $vec-ptr:(int32_t* qcs');
                int i;
                for (i = 0; i < len; ++i) {
                    int ix = 256*i - minQ + qs[i];
                    ++qcs_[ix];
                }
            }|]
            unsafeIncrement stats 0
            VUM.unsafeModify stats (min len) 1
            VUM.unsafeModify stats (max len) 2
            return ()

        getNoCaseV c p = c VS.! ord p + c VS.! (ord . toUpper $ p)

        findMinQValue :: [VU.Vector Int] -> Int
        findMinQValue = (flip (-) minQualityValue) . minimum . map findMinQValue'
        findMinQValue' :: VU.Vector Int -> Int
        findMinQValue' qs = fromMaybe 256 (VU.findIndex (/= 0) qs)

interleaveFQs :: (MonadError NGError m, MonadResource m, MonadUnliftIO m, MonadThrow m) => ReadSet -> C.ConduitT () B.ByteString m ()
interleaveFQs (ReadSet pairs singletons) = do
            sequence_ [interleavePair f0 f1 | (FastQFilePath _ f0, FastQFilePath _ f1) <- pairs]
            sequence_ [conduitPossiblyCompressedFile f | FastQFilePath _ f <- singletons]
    where
        interleavePair :: (MonadError NGError m, MonadResource m, MonadUnliftIO m, MonadThrow m) => FilePath -> FilePath -> C.ConduitT () B.ByteString m ()
        interleavePair f0 f1 =
                ((conduitPossiblyCompressedFile f0 .| linesC .| CL.chunksOf 4) `zipSources` (conduitPossiblyCompressedFile f1 .| linesC .| CL.chunksOf 4))
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




