{- Copyright 2013-2017 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Utils.Conduit
    ( ByteLine(..)
    , byteLineSinkHandle
    , conduitPossiblyCompressedFile
    , asyncMapC
    , asyncMapEitherC
    , linesC
    , linesCBounded
    , groupC
    , awaitJust
    , asyncGzipTo
    , asyncGzipToFile
    , asyncGzipFrom
    , asyncGzipFromFile
    , zipSink2
    , zipSource2
    ) where

import qualified Data.ByteString as B
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ
import           Control.Concurrent.STM (atomically)

import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.TQueue as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
#ifndef WINDOWS
-- bzlib cannot compile on Windows (as of 2016/07/05)
import qualified Data.Conduit.BZlib as CZ
#endif
import qualified Data.Conduit as C
import           Data.Conduit ((=$=), ($$))

import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), ViewL(..))
import           Control.Monad (unless, forM_, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Exception (finally, evaluate)
import           Control.DeepSeq
import           System.IO
import           Data.List (isSuffixOf)

import NGLess.NGError

-- | This just signals that a "line" is expected.
newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }

linesBounded maxLineSize = continue 0 []
    where
        continue n toks
            | n > maxLineSize = throwDataError ("Line too long (longer than " ++ show maxLineSize ++ " characters.")
            | otherwise = C.await >>= \case
                    Nothing -> when (n > 0) $ C.yield (B.concat $ reverse toks)
                    Just tok -> emit n toks tok
        emit n toks tok = case B.elemIndex 10 tok of
                Nothing -> continue (n + B.length tok) (tok:toks)
                Just ix -> let (start,rest) = B.splitAt ix tok in do
                                C.yield (B.concat $ reverse (start:toks))
                                emit 0 [] (B.tail rest)

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC =
    CB.lines
#ifdef WINDOWS
        =$= CL.map (\line ->
                        if not (B.null line) && B.index line (B.length line - 1) == 13
                            then B.take (B.length line - 1) line
                            else line)
#endif
        =$= CL.map ByteLine
{-# INLINE linesC #-}


linesCBounded :: (MonadError NGError m) => C.Conduit B.ByteString m ByteLine
linesCBounded =
    linesBounded 8192
#ifdef WINDOWS
        =$= CL.map (\line ->
                        if not (B.null line) && B.index line (B.length line - 1) == 13
                            then B.take (B.length line - 1) line
                            else line)
#endif
        =$= CL.map ByteLine

byteLineSinkHandle :: (MonadIO m) => Handle -> C.Sink ByteLine m ()
byteLineSinkHandle h = CL.map unwrapByteLine =$= C.unlinesAscii =$= C.sinkHandle h


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
            v :< rest -> (liftIO (A.wait v) >>= yieldOrCleanup rest) >> yAll rest

        loop :: Seq.Seq (A.Async b) -> C.Conduit a m b
        loop q = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                    v' <- sched v
                    case Seq.viewl q of
                        (r :< rest) -> do
                            yieldOrCleanup rest =<< liftIO (A.wait r)
                            loop (rest |> v')
                        _ -> error "should never happen"
        cleanup :: Seq.Seq (A.Async b) -> m ()
        cleanup q = liftIO $ forM_ q A.cancel
        yieldOrCleanup q = flip C.yieldOr (cleanup q)

-- | asyncMapC with error handling. The inner function can now return an error
-- (as a 'Left'). When the first error is seen, it 'throwError's in the main
-- monad. Note that 'f' may be evaluated for arguments beyond the first error
-- (as some threads may be running in the background and already processing
-- elements after the first error).
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

-- | concatenates input into larger chunks and yields it. Its indended use is
-- to build up larger blocks from smaller ones so that they can be sent across
-- thread barriers with little overhead.
--
-- the chunkSize parameter is a hint, not an exact element. In particular,
-- larger chunks are not split up and smaller chunks can be yielded too.
bsConcatTo :: (MonadIO m) => Int -- ^ chunk hint
                            -> C.Conduit B.ByteString m [B.ByteString]
bsConcatTo chunkSize = awaitJust start
    where
        start v
            | B.length v >= chunkSize = C.yield [v] >> bsConcatTo chunkSize
            | otherwise = continue [v] (B.length v)
        continue chunks s = C.await >>= \case
            Nothing -> C.yield chunks
            Just v
                | B.length v + s > chunkSize -> C.yield chunks >> start v
                | otherwise -> continue (v:chunks) (s + B.length v)

-- | A simple sink which performs gzip in a separate thread and writes the results to `h`.
asyncGzipTo :: forall m. (MonadIO m) => Handle -> C.Sink B.ByteString m ()
asyncGzipTo h = do
    -- We allocate the queue separately (instead of using CA.paired*) so that
    -- `src` and `sink` end up with different underlying monads (src is a
    -- conduit over IO, while sink is over m):
    q <- liftIO $ TQ.newTBMQueueIO 4
    let src :: C.Source IO [B.ByteString]
        src = CA.sourceTBMQueue q
    consumer <- liftIO $ A.async (src $$ CL.map (B.concat . reverse) =$= CZ.gzip =$= C.sinkHandle h)
    liftIO $ A.link consumer -- if there is an error in writing, then the queue would never be read from and we could deadlock
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
asyncGzipFrom :: forall m. (MonadIO m, MonadResource m) => Handle -> C.Source m B.ByteString
asyncGzipFrom h = do
    let allocate = do
            -- We allocate the queue separately (instead of using CA.paired*) so that
            -- `src` and `sink` end up with different underlying monads (sink is a
            -- conduit over IO, while we are in m)
            q <- TQ.newTBMQueueIO 4
            producer <- A.async $
                    (C.sourceHandle h =$= CZ.multiple CZ.ungzip $$ CA.sinkTBMQueue q False)
                    `finally`
                    atomically (TQ.closeTBMQueue q)
            return (q, producer)
    C.bracketP
        allocate
        (liftIO . A.cancel . snd)
        $ \(q,pr) -> do
            CA.sourceTBMQueue q
            liftIO $ A.wait pr

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
#ifndef WINDOWS
    | ".bz2" `isSuffixOf` fname = C.sourceFile fname =$= CZ.bunzip2
#else
    | ".bz2" `isSuffixOf` fname = error "bzip2 decompression is not available on Windows"
#endif
    | otherwise = C.sourceFile fname

