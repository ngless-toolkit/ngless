{- Copyright 2013-2016 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Conduit
    ( ByteLine(..)
    , conduitPossiblyCompressedFile
    , asyncMapC
    , asyncMapEitherC
    , linesC
    , groupC
    , awaitJust
    , bsConcatTo
    , asyncGzipTo
    , asyncGzipToFile
    , asyncGzipFrom
    , asyncGzipFromFile
    , zipSink2
    , zipSource2
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI -- not a fully kosher import
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ

import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.TQueue as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.BZlib as CZ
import qualified Data.Conduit as C
import           Data.Conduit ((=$=), ($$))

import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), ViewL(..))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Exception (evaluate)
import           Control.DeepSeq
import           Foreign.ForeignPtr
import           System.IO
import           Data.Word
import           Foreign.Ptr
import           Data.List (isSuffixOf)

newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC = CB.lines =$= CL.map ByteLine
{-# INLINE linesC #-}

-- | This is like Data.Conduit.List.map, except that each element is processed
-- in a separate thread (up to maxSize can be queued up at any one time).
-- Results are evaluated to normal form (not WHNF!) to ensure that the
-- computation is fully evaluated before being yielded to the next conduit.
asyncMapC :: forall a m b . (MonadIO m, NFData b) => Int -> (a -> b) -> C.Conduit a m b
asyncMapC maxSize f = initLoop (0 :: Int) (Seq.empty :: Seq.Seq (A.Async b))
    where
        initLoop :: Int -> Seq.Seq (A.Async b) -> C.Conduit a m b
        initLoop size q
            | size == maxSize = loop q
            | otherwise = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                        v' <- sched v
                        initLoop (size + 1) (q |> v')
        sched :: a -> C.ConduitM a b m (A.Async b)
        sched v = liftIO . A.async . evaluate . force $ f v

        -- | yield all
        yAll :: Seq.Seq (A.Async b) -> C.Conduit a m b
        yAll q = case Seq.viewl q of
            EmptyL -> return ()
            v :< rest -> (liftIO (A.wait v) >>= C.yield) >> yAll rest

        loop :: Seq.Seq (A.Async b) -> C.Conduit a m b
        loop q = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                    v' <- sched v
                    case Seq.viewl q of
                        (r :< rest) -> do
                            C.yield =<< liftIO (A.wait r)
                            loop (rest |> v')
                        _ -> error "should never happen"

-- | asyncMapC with error handling. The inner function can now return an error
-- (as a 'Left'). When the first error is seen, it 'throwError's in the main
-- monad. Note that if 'f' is not pure, then 'f' may be evaluated for arguments
-- beyond the first error (as this is being performed in a separate thread.
asyncMapEitherC :: forall a m b e . (MonadIO m, NFData b, NFData e, MonadError e m) => Int -> (a -> Either e b) -> C.Conduit a m b
asyncMapEitherC maxSize f = asyncMapC maxSize f =$= (C.awaitForever $ \case
                                Right v -> C.yield v
                                Left err -> throwError err)

-- | groupC yields the input as groups of 'n' elements. If the input is not a
-- multiple of 'n', the last element will be incomplete
groupC :: (Monad m) => Int -> C.Conduit a m [a]
groupC n = loop n []
    where
        loop 0 ps = C.yield (reverse ps) >> loop n []
        loop c ps = C.await >>= \case
            Nothing -> unless (null ps) $ C.yield (reverse ps)
            Just p -> loop (c-1) (p:ps)

-- | This is a simple utility adapted from
-- http://neilmitchell.blogspot.de/2015/07/thoughts-on-conduits.html
awaitJust :: Monad m => (i -> C.Conduit i m o) -> C.Conduit i m o
awaitJust f = C.await >>= \case
        Nothing -> return ()
        Just v -> f v

bsConcatTo :: (Monad m, MonadIO m) => Int -> C.Conduit B.ByteString m B.ByteString
bsConcatTo chunkSize = awaitJust start
    where
        start v
            | B.length v >= chunkSize = C.yield v >> bsConcatTo chunkSize
            | otherwise = do
                buff <- liftIO $ allocateBuffer chunkSize
                liftIO $ copyTo buff 0 v
                continue buff (B.length v)
        continue buff nextPos = C.await >>= \case
            Nothing -> C.yield (freeze buff nextPos)
            Just v
                | B.length v + nextPos > chunkSize -> C.yield (freeze buff nextPos) >> start v
                | otherwise -> do
                    liftIO $ copyTo buff nextPos v
                    continue buff (nextPos + B.length v)

        allocateBuffer :: Int -> IO (ForeignPtr Word8)
        allocateBuffer n = mallocForeignPtrBytes n

        copyTo :: (ForeignPtr Word8) -> Int -> B.ByteString -> IO ()
        copyTo fp offset src = let (src_fp,src_off,src_size) = BI.toForeignPtr src in
                 withForeignPtr fp $ \p -> withForeignPtr src_fp $ \src_p ->
                    BI.memcpy (p `plusPtr` offset) (src_p `plusPtr` src_off) src_size

        freeze :: (ForeignPtr Word8) -> Int -> B.ByteString
        freeze p size = BI.fromForeignPtr p 0 size

-- | A simple sink which performs gzip in a separate thread and writes the results to `h`.
asyncGzipTo :: forall m. (MonadIO m) => Handle -> C.Sink B.ByteString m ()
asyncGzipTo h = do
    -- We allocate the queue separately (instead of using CA.paired*) so that
    -- `src` and `sink` end up with different underlying monads (src is a
    -- conduit over IO, while sink is over m):
    q <- liftIO $ TQ.newTBMQueueIO 4
    let src :: C.Source IO B.ByteString
        src = CA.sourceTBMQueue q
    consumer <- liftIO $ A.async (src $$ CZ.gzip =$= C.sinkHandle h)
    bsConcatTo ((2 :: Int) ^ (15 :: Int)) =$= CA.sinkTBMQueue q True
    liftIO (A.wait consumer)

asyncGzipToFile :: forall m. (MonadIO m, MonadResource m) => FilePath -> C.Sink B.ByteString m ()
asyncGzipToFile fname = C.bracketP
    (openFile fname WriteMode)
    hClose
    asyncGzipTo

-- | A source which ungzipped from the the given handle. Note that this "reads
-- ahead" so if you do not use all the input, the Handle will probably be left
-- at an undefined position in the file.
asyncGzipFrom :: forall m. (MonadIO m) => Handle -> C.Source m B.ByteString
asyncGzipFrom h = do
    -- We allocate the queue separately (instead of using CA.paired*) so that
    -- `src` and `sink` end up with different underlying monads (sink is a
    -- conduit over IO, while we are in m)
    q <- liftIO $ TQ.newTBMQueueIO 4
    producer <- liftIO $ A.async (C.sourceHandle h =$= CZ.multiple CZ.ungzip $$ CA.sinkTBMQueue q True)
    CA.sourceTBMQueue q
    liftIO (A.cancel producer)

asyncGzipFromFile :: forall m. (MonadIO m, MonadResource m) => FilePath -> C.Source m B.ByteString
asyncGzipFromFile fname = C.bracketP
    (openFile fname ReadMode)
    hClose
    asyncGzipFrom

zipSource2 a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)

zipSink2 :: (Monad m) => C.Sink i m a -> C.Sink i m b -> C.Sink i m (a,b)
zipSink2 a b = C.getZipSink((,) <$> C.ZipSink a <*> C.ZipSink b)

conduitPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = asyncGzipFromFile fname
    | ".bz2" `isSuffixOf` fname = C.sourceFile fname =$= CZ.bunzip2
    | otherwise = C.sourceFile fname

