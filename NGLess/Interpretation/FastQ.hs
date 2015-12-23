{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Interpretation.FastQ
    ( executeQProc
    , optionalSubsample
    , writeTempFastQ
    ) where

import System.IO
import Data.List
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($=), ($$), (=$=), (=$))

import FileManagement
import Data.FastQStatistics
import Data.FastQ
import Configuration
import Language
import Output
import Utils.Utils
import NGLess

writeTempFastQ :: FilePath -> [ShortRead] -> FastQEncoding -> NGLessIO FilePath
writeTempFastQ fn rs enc = do
    (newfp,h) <- openNGLTempFile fn "" "fq.gz"
    liftIO $ do
        hWriteGZIP h (asFastQ enc rs)
        hClose h
    return newfp

drop100 = loop (0 :: Int)
    where
        loop 400 = loop 0
        loop n
            | n < 4 = do
                mline <- C.await
                case mline of
                    Just line -> C.yield line >> loop (n+1)
                    Nothing -> return ()
            | otherwise = C.await >> loop (n+1)

uncompressC f
    | ".gz" `isSuffixOf` f = C.ungzip
    | otherwise = C.awaitForever C.yield

optionalSubsample :: FilePath -> NGLessIO FilePath
optionalSubsample f = do
    subsampleActive <- nConfSubsample <$> nglConfiguration
    if not subsampleActive
        then return f
        else do
            outputListLno' TraceOutput ["Subsampling file ", f]
            (newfp,h) <- openNGLTempFile f "" "fq.gz"
            C.sourceFile f
                $= uncompressC f
                =$= CB.lines
                =$= drop100
                =$= C.unlinesAscii
                =$= C.gzip
                $$ CB.sinkHandle h
            liftIO $ hClose h
            outputListLno' TraceOutput ["Finished subsampling (temp sampled file is ", newfp, ")"]
            return newfp

-- ^ Process quality.
executeQProc :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO NGLessObject
executeQProc enc f = do
        fd <- liftIO $ statsFromFastQ <$> readPossiblyCompressedFile f
        enc' <- case enc of
                Just e -> return e
                Nothing -> guessEncoding (lc fd)
        liftIO $ outputFQStatistics f fd enc'
        p "Simple Statistics completed for: " f
        p "Number of base pairs: "      (show $ length (qualCounts fd))
        p "Encoding is: "               (show enc')
        p "Number of sequences: "   (show $ nSeq fd)
        return $ NGOReadSet1 enc' f
    where
        p s0 s1  = outputListLno' DebugOutput [s0, s1]

