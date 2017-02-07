{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , FQStatistics(..)
    , srSlice
    , encodingFor
    , srLength
    , encodingOffset
    , encodingName
    , fqDecode
    , fqDecodeC
    , fqEncode
    , fqEncodeC
    , gcFraction
    , statsFromFastQ
    , fqStatsC
    , qualityPercentiles
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Control.DeepSeq (NFData(..))
import           Data.Strict.Tuple (Pair(..))
import Data.Conduit         (($$), (=$=))
import Control.Monad
import Control.Monad.Except
import Control.Exception

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed.Mutable as VUM

import Foreign.Ptr
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
        , srQualities :: !(VU.Vector Int8) -- ^ these have been decoded
        } deriving (Eq, Show, Ord)

instance NFData ShortRead where
    rnf ShortRead{} = ()

data FastQEncoding = SangerEncoding | SolexaEncoding deriving (Eq, Bounded, Enum, Show, Ord)

data FQStatistics = FQStatistics
                { bpCounts :: (Int, Int, Int, Int) -- ^ number of (A, C, T, G)
                , lc :: Int8 -- ^ lowest quality value
                , qualCounts ::  [V.Vector Int] -- ^ quality counts by position
                , nSeq :: Int -- ^ number of sequences
                , seqSize :: (Int,Int) -- ^ min and max
                } deriving(Eq,Show)

instance NFData FQStatistics where
    rnf (FQStatistics (!_,!_,!_,!_) !_ qv !_ (!_,!_)) = rnf qv

minQualityValue :: Num a => a
minQualityValue = -5

srLength = B.length . srSequence

srSlice :: Int -> Int -> ShortRead -> ShortRead
srSlice s n (ShortRead rId rS rQ) = assert (B.length rS >= s + n) $ ShortRead rId (B.take n $ B.drop s rS) (VU.slice s n rQ)

encodingOffset :: Num a => FastQEncoding -> a
encodingOffset SangerEncoding = 33
encodingOffset SolexaEncoding = 64

encodingName :: FastQEncoding -> String
encodingName SangerEncoding = "Sanger (33 offset)"
encodingName SolexaEncoding = "Solexa (64 offset)"

guessEncoding :: (MonadError NGError m) => Word8 -> m FastQEncoding
guessEncoding lowC
    | lowC < 33 = throwDataError ("No known encodings with chars < 33 (Yours was "++ show lowC ++ ")")
    | lowC < 58 = return SangerEncoding
    | otherwise = return SolexaEncoding

-- | encode as a string
fqEncodeC :: (Monad m) => FastQEncoding -> C.Conduit ShortRead m B.ByteString
fqEncodeC enc = CL.map (fqEncode enc)

-- Using B.map instead of this function makes this loop be one of the functions
-- with the highest memory allocation in ngless.
bsAdd :: VU.Vector Int8 -> Int8 -> B.ByteString
bsAdd c delta = BI.unsafeCreate cn $ \p -> copyAddLoop p 0
    where
        cn = VU.length c
        copyAddLoop p i
            | i == cn = return ()
            | otherwise = do
                poke (p `plusPtr` i) ((fromIntegral $ c VU.! i + delta) :: Word8)
                copyAddLoop p (i + 1)

vSub :: B.ByteString -> Int8 -> VU.Vector Int8
vSub qs delta = VU.generate (B.length qs) $ \i -> fromIntegral (B.index qs i) - delta

fqEncode :: FastQEncoding -> ShortRead -> B.ByteString
fqEncode enc (ShortRead a b c) = B.concat [a, "\n", b, "\n+\n", bsAdd c offset, "\n"]
    where
        offset :: Int8
        offset = encodingOffset enc

fqDecodeC :: (Monad m, MonadError NGError m) => FastQEncoding -> C.Conduit ByteLine m ShortRead
fqDecodeC enc = groupC 4 =$= CL.mapM parseShortReads
    where
        offset :: Int8
        offset = encodingOffset enc
        parseShortReads [ByteLine rid, ByteLine rseq, _, ByteLine rqs] = return $! ShortRead rid rseq (vSub rqs offset)
        parseShortReads _ = throwDataError "Number of lines in FastQ file is not multiple of 4! EOF found"

-- | reads a sequence of short reads.
-- returns an error if there are any problems.
--
-- Note that the result of this function is not lazy! It will consume the whole
-- input before it produces the first output (because it needs to determine
-- whether an error occurred).
--
-- See 'fqDecodeC'
fqDecode :: FastQEncoding -> BL.ByteString -> NGLess [ShortRead]
fqDecode enc s = C.runConduit $
    CL.sourceList (BL.toChunks s)
        =$= linesCBounded
        =$= fqDecodeC enc
        =$= CL.consume

statsFromFastQ :: FilePath -> FastQEncoding -> NGLessIO FQStatistics
statsFromFastQ fp enc =
        conduitPossiblyCompressedFile fp
            =$= linesCBounded
            =$= getPairedLines
            $$ fqStatsC
    where
        offset = encodingOffset enc
        getPairedLines :: C.Conduit ByteLine NGLessIO (Pair ByteLine (VU.Vector Int8))
        getPairedLines = groupC 4 =$= CL.mapM getPairedLines'
            where
                getPairedLines' [_, bps, _, ByteLine qs] = return $! bps :!: vSub qs offset
                getPairedLines' _ = throwDataError "fastq lines are not a multiple of 4"

-- | Guess the encoding of a file
encodingFor :: FilePath -> NGLessIO FastQEncoding
encodingFor fp = do
    let countMin :: (Int, Word8) -> Word8 -> (Int, Word8)
        countMin (!c,!m) m' = (c+1, min m m')
        minLc :: (MonadError NGError m) => [ByteLine] -> m Word8
        minLc [_,_,_,qs] = return . B.minimum . unwrapByteLine $ qs
        minLc _ = throwDataError ("Malformed FASTQ file: '" ++ fp ++ "': number of lines is not a multiple of 4")

    (c,m) <- conduitPossiblyCompressedFile fp
        =$= linesCBounded
        =$= groupC 4
        =$= CL.isolate 100
        =$= CL.mapM minLc
        $$ CL.fold countMin (0,maxBound :: Word8)
    when (c < 1) $
        throwDataError ("Cannot determine encoding for input file '" ++ fp ++ "'. File is empty.")
    guessEncoding m


fqStatsC :: forall m. (MonadIO m) => C.Sink (Pair ByteLine (VU.Vector Int8)) m FQStatistics
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
            let lcT = findMinQValue qcs'
            aCount <- getNoCaseV charCounts 'a'
            cCount <- getNoCaseV charCounts 'c'
            gCount <- getNoCaseV charCounts 'g'
            tCount <- getNoCaseV charCounts 't'
            return $! FQStatistics (aCount, cCount, gCount, tCount) (fromIntegral lcT) qcs' n (minSeq, maxSeq)
    where

        update :: VSM.IOVector Int -> VUM.IOVector Int -> IORef (VSM.IOVector Int) -> Pair ByteLine (VU.Vector Int8) -> m ()
        update !charCounts !stats qcs (ByteLine bps :!: qs) = liftIO $ do
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
                        let qi = 256*i - minQualityValue + fromIntegral (qs VU.! i)
                        VSM.unsafeModify qcs' (+ 1) qi
                        updateQCounts n (i+1)
            updateQCounts (toEnum len) 0
            unsafeIncrement stats 0
            VUM.unsafeModify stats (min len) 1
            VUM.unsafeModify stats (max len) 2
            return ()

        getNoCaseV c p = do
            lower <- VSM.read c (ord p)
            upper <- VSM.read c (ord . toUpper $ p)
            return (lower + upper)
        findMinQValue :: [VU.Vector Int] -> Int
        findMinQValue = (flip (-) minQualityValue) . minimum . map findMinQValue'
        findMinQValue' :: VU.Vector Int -> Int
        findMinQValue' qs = fromMaybe 256 (VU.findIndex (/= 0) qs)

gcFraction :: FQStatistics -> Double
gcFraction res = gcCount / allBpCount
    where
        (bpA,bpC,bpG,bpT) = bpCounts res
        gcCount = fromIntegral $ bpC + bpG
        allBpCount = fromIntegral $ bpA + bpC + bpG + bpT



qualityPercentiles :: FQStatistics -> [(Int, Int, Int, Int)]
qualityPercentiles FQStatistics{qualCounts=qCounts} = Prelude.map statistics qCounts
    where
        statistics :: V.Vector Int -> (Int, Int, Int, Int)
        statistics qs = (bpSum `div` elemTotal + minQualityValue
                                , calcPercentile 0.50 + minQualityValue
                                , calcPercentile 0.25 + minQualityValue
                                , calcPercentile 0.75 + minQualityValue)
            where
                -- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2.
                -- 'a' and 'b' minus encoding.
                bpSum = V.ifoldl' (\n i q -> n + i * q) 0 qs
                elemTotal = V.sum qs

                calcPercentile :: Double -> Int
                calcPercentile perc = accUntilLim val'
                    where
                        val' = ceiling (fromIntegral elemTotal * perc)
                        accUntilLim :: Int -> Int
                        accUntilLim lim = case V.findIndex (>= lim) $ V.postscanl (+) 0 qs of
                              Just v -> v
                              Nothing -> error "ERROR: Logical impossibility in calcPercentile function"




