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

linesBounded:: (Monad m, MonadError NGError m) => Int -> C.Conduit B.ByteString m B.ByteString
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


-- | Remove trailing \r present when the original line terminator was \r\n (windows)
lineWindowsTerminated :: B.ByteString -> B.ByteString
lineWindowsTerminated line = if not (B.null line) && B.index line (B.length line - 1) == carriage_return
                                then B.take (B.length line - 1) line
                                else line
                                    where carriage_return = 13
{-# INLINE lineWindowsTerminated #-}


linesUnBoundedC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesUnBoundedC =
    CB.lines
        .| CL.map lineWindowsTerminated
        .| CL.map ByteLine
{-# INLINE linesUnBoundedC #-}


linesC :: (MonadError NGError m) => C.Conduit B.ByteString m ByteLine
linesC =
    linesBounded 65536
        .| CL.map (ByteLine . lineWindowsTerminated)
{-# INLINE linesC #-}

byteLineSinkHandle :: (MonadIO m) => Handle -> C.Sink ByteLine m ()
byteLineSinkHandle h = CL.mapM_ (\(ByteLine val) -> liftIO (B.hPut h val >> B.hPut h nl))
    where
        nl = B.singleton 10
{-# INLINE byteLineSinkHandle #-}

zipSource2 a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)

zipSink2 :: (Monad m) => C.Sink i m a -> C.Sink i m b -> C.Sink i m (a,b)
zipSink2 a b = C.getZipSink((,) <$> C.ZipSink a <*> C.ZipSink b)

