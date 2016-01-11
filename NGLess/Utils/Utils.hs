{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utils.Utils
    ( conduitPossiblyCompressedFile
    , lookupWithDefault
    , maybeM
    , uniq
    , readPossiblyCompressedFile
    , readProcessErrorWithExitCode
    , hWriteGZIP
    , allSame
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Data.Conduit.Combinators as C
import Data.Conduit (($=))
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.BZlib as CZ
import Control.Exception (evaluate)
import Control.Monad
import Control.Concurrent
import System.IO
import System.Process

import Data.List (isSuffixOf, group)
import Data.Maybe (fromMaybe)

lookupWithDefault :: Eq b => a -> b -> [(b,a)] -> a
lookupWithDefault def key values = fromMaybe def $ lookup key values

uniq :: Eq a => [a] -> [a]
uniq = map head . group

readPossiblyCompressedFile ::  FilePath -> IO BL.ByteString
readPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = GZip.decompress <$> BL.readFile fname
    | ".bz2" `isSuffixOf` fname = BZip.decompress <$> BL.readFile fname
    | otherwise = BL.readFile fname


hWriteGZIP :: Handle -> BL.ByteString -> IO ()
hWriteGZIP h = BL.hPut h . GZip.compress

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (e:es) = all (==e) es


readProcessErrorWithExitCode cp = do
    (_, _, Just herr, jHandle) <-
        createProcess cp { std_err = CreatePipe }
    err <- hGetContents herr
    -- In a separate thread, consume all the error input
    -- the same pattern is used in the implementation of
    -- readProcessWithErrorCode (which cannot be used here as we want to
    -- use `hout` for stdout)
    void . forkIO $ void (evaluate (length err))
    exitCode <- waitForProcess jHandle
    hClose herr
    return (err, exitCode)

maybeM :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
maybeM ma f = ma >>= \case
    Nothing -> return Nothing
    Just a -> f a

conduitPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = C.sourceFile fname $= CZ.ungzip
    | ".bz2" `isSuffixOf` fname = C.sourceFile fname $= CZ.bunzip2
    | otherwise = C.sourceFile fname
