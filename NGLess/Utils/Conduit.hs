{- Copyright 2013-2018 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, CPP #-}

module Utils.Conduit
    ( ByteLine(..)
    , byteLineSinkHandle
    , conduitPossiblyCompressedFile
    , asyncMapC
    , asyncMapEitherC
    , linesUnBoundedC
    , linesC
    , awaitJust
    , asyncGzipTo
    , asyncGzipToFile
    , asyncGzipFrom
    , asyncGzipFromFile
    , zipSink2
    , zipSource2
    ) where

import qualified Data.ByteString as B

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit ((.|))

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Error.Class (MonadError(..))
import           System.IO

import           Data.Conduit.Algorithms.Utils (awaitJust)
import           Data.Conduit.Algorithms.Async

import NGLess.NGError

-- | This just signals that a "line" is expected.
newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }
                deriving (Show)

maxLineSize :: Int
maxLineSize = 65536

concatrevline :: B.ByteString -> [B.ByteString] -> ByteLine
concatrevline line [] = ByteLine $ lineWindowsTerminated line
concatrevline line toks = ByteLine . lineWindowsTerminated $ B.concat (reverse (line:toks))
{-# INLINE concatrevline #-}

linesC:: (Monad m, MonadError NGError m) => C.ConduitT B.ByteString ByteLine m ()
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


linesUnBoundedC :: (Monad m) => C.ConduitT B.ByteString ByteLine m ()
linesUnBoundedC =
    CB.lines
        .| CL.map lineWindowsTerminated
        .| CL.map ByteLine
{-# INLINE linesUnBoundedC #-}



byteLineSinkHandle :: (MonadIO m) => Handle -> C.ConduitT ByteLine C.Void m ()
byteLineSinkHandle h = CL.mapM_ (\(ByteLine val) -> liftIO (B.hPut h val >> B.hPut h nl))
    where
        nl = B.singleton 10
{-# INLINE byteLineSinkHandle #-}

zipSource2 :: Monad m => C.ConduitT () a m () -> C.ConduitT () b m () -> C.ConduitT () (a,b) m ()
zipSource2 a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)
{-# INLINE zipSource2 #-}

zipSink2 :: (Monad m) => C.ConduitT i C.Void m a -> C.ConduitT i C.Void m b -> C.ConduitT i C.Void m (a,b)
zipSink2 a b = C.getZipSink((,) <$> C.ZipSink a <*> C.ZipSink b)
{-# INLINE zipSink2 #-}

