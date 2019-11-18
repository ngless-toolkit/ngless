{- Copyright 2013-2019 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Utils.Conduit
    ( ByteLine(..)
    , byteLineSinkHandle
    , byteLineVSinkHandle
    , linesC
    , linesVC
    , zipSink2
    , zipSource2
    ) where

import qualified Data.ByteString as B

import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Error.Class (MonadError(..))
import           System.IO

import NGLess.NGError

-- | This just signals that a "line" is expected.
newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }
                deriving (Show)

-- A limit was introduced to avoid running out of memory on corrupt files, but
-- it needs to be large enough to accommodate nanopore reads, see:
-- https://groups.google.com/forum/#!topic/ngless/-ovfYW8hfAs
maxLineSize :: Int
maxLineSize = 1024 * 1024 * 1024

concatrevline :: B.ByteString -> [B.ByteString] -> ByteLine
concatrevline line [] = ByteLine $ lineWindowsTerminated line
concatrevline line toks = ByteLine . lineWindowsTerminated $ B.concat (reverse (line:toks))
{-# INLINE concatrevline #-}

linesC:: (MonadError NGError m) => C.ConduitT B.ByteString ByteLine m ()
linesC = continue 0 []
    where
        continue n toks
            | n > maxLineSize = throwDataError ("Line too long (length is " ++ show n ++ " characters).")
            | otherwise = C.await >>= maybe
                                        (when (n > 0) $ C.yield (concatrevline B.empty toks))
                                        (emit n toks)
        emit n toks tok = case B.elemIndex 10 tok of
                Nothing -> continue (n + B.length tok) (tok:toks)
                Just ix -> let (start,rest) = B.splitAt ix tok in do
                                C.yield (concatrevline start toks)
                                emit 0 [] (B.tail rest)

{-# INLINE linesC #-}

-- | Remove trailing \r present when the original line terminator was \r\n (windows)
lineWindowsTerminated :: B.ByteString -> B.ByteString
lineWindowsTerminated line = if not (B.null line) && B.index line (B.length line - 1) == carriage_return
                                then B.take (B.length line - 1) line
                                else line
                                    where carriage_return = 13
{-# INLINE lineWindowsTerminated #-}

-- | Equivalent to 'linesC .| CC.conduitVector nlines'
linesVC :: (MonadIO m, MonadError NGError m) => Int -> C.ConduitT B.ByteString (V.Vector ByteLine) m ()
linesVC nlines = do
            vec <- liftIO $ VM.new nlines
            continue vec 0 0 []
        where
            continue vec vix n toks
                | n > maxLineSize = throwDataError ("Line too long (length is " ++ show n ++ " characters).")
                | otherwise = C.await >>= maybe (finish vec vix toks) (emit vec vix n toks)

            finish   _   0 [] = return ()
            finish vec vix [] = C.yield =<< liftIO (V.unsafeSlice 0 vix <$> V.unsafeFreeze vec)
            finish vec vix toks = do
                    liftIO $ VM.write vec vix (ByteLine . lineWindowsTerminated . B.concat $ reverse toks)
                    finish vec (vix + 1) []

            emit vec vix n toks tok = do
                (done, vec', vix', n', toks') <- liftIO $ splitWrite [] vec vix n toks tok
                CL.sourceList (reverse done)
                continue vec' vix' n' toks'

            -- splitWrite is in IO. This is a micro-optimization, but this code
            -- can be in the inner loop, so it's worthwhile to micro-optimize.
            --
            -- Basically, moving up and down the transformer stack (with lift &
            -- friends) can be expensive. Doing the inner loop in IO is
            -- measurably faster.
            splitWrite done vec vix !n toks tok
                | vix >= nlines = do
                    f <- V.unsafeFreeze vec
                    vec' <- VM.new nlines
                    splitWrite (f:done) vec' 0 n toks tok
                | otherwise = case B.elemIndex 10 tok of
                    Nothing -> return (done, vec, vix, n + B.length tok, (if not (B.null tok) then (tok:toks) else toks))
                    Just ix -> do
                        let (start, rest) = B.splitAt ix tok
                        VM.write vec vix (concatrevline start toks)
                        splitWrite done vec (vix + 1) 0 [] (B.tail rest)
{-# INLINE linesVC #-}


byteLineSinkHandle :: (MonadIO m) => Handle -> C.ConduitT ByteLine C.Void m ()
byteLineSinkHandle h = CL.mapM_ (\(ByteLine val) -> liftIO (B.hPut h val >> B.hPut h nl))
    where
        nl = B.singleton 10
{-# INLINE byteLineSinkHandle #-}


byteLineVSinkHandle :: (MonadIO m) => Handle -> C.ConduitT (V.Vector ByteLine) C.Void m ()
byteLineVSinkHandle h = CL.mapM_ $ liftIO . V.mapM_ (\(ByteLine val) -> B.hPut h val >> B.hPut h nl)
    where
        nl = B.singleton 10
{-# INLINE byteLineVSinkHandle #-}


zipSource2 :: Monad m => C.ConduitT () a m () -> C.ConduitT () b m () -> C.ConduitT () (a,b) m ()
zipSource2 a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)
{-# INLINE zipSource2 #-}


zipSink2 :: (Monad m) => C.ConduitT i C.Void m a -> C.ConduitT i C.Void m b -> C.ConduitT i C.Void m (a,b)
zipSink2 a b = C.getZipSink((,) <$> C.ZipSink a <*> C.ZipSink b)
{-# INLINE zipSink2 #-}

