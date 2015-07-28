{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module ProcessFastQ
    (
    writeReadSet,
    executeQProc,
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Compression.GZip as GZip

import System.IO
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import FileManagement
import FastQStatistics
import Data.FastQ
import Language
import Output
import Utils.Utils
import NGLess

writeGZIP :: Handle -> BL.ByteString -> IO ()
writeGZIP h = BL.hPut h . GZip.compress

writeReadSet :: FilePath -> [ShortRead] -> FastQEncoding -> NGLessIO FilePath
writeReadSet fn rs enc = do
    (newfp,h) <- openNGLTempFile fn "" "fq.gz"
    liftIO $ do
        writeGZIP h (asFastQ enc rs)
        hClose h
    return newfp

-- ^ Process quality.
executeQProc :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> FilePath         -- ^ destination for statistics
                -> NGLessIO NGLessObject
executeQProc enc f dst = do
        fd <- liftIO $ computeStats <$> readPossiblyCompressedFile f
        let enc' = fromMaybe (guessEncoding . lc $ fd) enc
        liftIO $ outputFQStatistics f fd enc'
        p "Generation of statistics for " dst
        p "Simple Statistics completed for: " dst
        p "Number of base pairs: "      (show $ length (qualCounts fd)) 
        p "Encoding is: "               (show enc')
        p "Number of sequences: "   (show $ nSeq fd)
        p "Loaded file: " f
        return $ NGOReadSet1 enc' f
    where
        p s0 s1  = outputListLno' DebugOutput [s0, s1]

