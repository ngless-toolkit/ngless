{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Interpretation.FastQ
    ( executeFastq
    , executeGroup
    , executeQualityProcess
    , optionalSubsample
    , writeTempFastQ
    ) where

import System.IO
import Data.List
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($=), ($$), (=$=))

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
doQC1 :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO ReadSet
doQC1 enc f = do
        fd <- liftIO $ statsFromFastQ <$> readPossiblyCompressedFile f
        enc' <- case enc of
                Just e -> return e
                Nothing -> guessEncoding (lc fd)
        liftIO $ outputFQStatistics f fd enc'
        p "Simple Statistics completed for: " f
        p "Number of base pairs: "      (show $ length (qualCounts fd))
        p "Encoding is: "               (show enc')
        p "Number of sequences: "   (show $ nSeq fd)
        return (ReadSet1 enc' f)
    where
        p s0 s1  = outputListLno' DebugOutput [s0, s1]

executeGroup :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeGroup (NGOList rs) args = do
        name <- lookupStringOrScriptError "group call" "name" args
        rs' <- getRSOrError `mapM` rs
        (newfp, h) <- openNGLTempFile (T.unpack name) "concatenated_" "fq"
        catFiles h rs'
        liftIO (hClose h)
        let NGOReadSet (ReadSet1 enc _) = head rs
        return (NGOSample name [ReadSet1 enc newfp])
    where
        getRSOrError (NGOReadSet r) = return r
        getRSOrError other = throwShouldNotOccur . concat $ ["In group call, all arguments should have been NGOReadSet! Got ", show other]
executeGroup other _ = throwScriptError ("Illegal argument to group(): " ++ show other)

catFiles h fs = forM_ fs $ \(ReadSet1 _ fp) -> catFile fp
    where
        catFile fp = liftIO (BL.readFile fp >>= BL.hPut h)

executeFastq :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeFastq expr args = do
    encName <- lookupSymbolOrScriptErrorDef (return "auto") "fastq arguments" "encoding" args
    enc <- case encName of
            "auto" -> return Nothing
            "33" -> return $ Just SangerEncoding
            "sanger" -> return $ Just SangerEncoding
            "64" -> return $ Just SolexaEncoding
            "solexa" -> return $ Just SolexaEncoding
            other -> throwScriptError ("Unkown encoding for fastq " ++ T.unpack other)
    case expr of
        (NGOString fname) -> NGOReadSet <$> do
            let fp = T.unpack fname
            doQC1 enc =<< optionalSubsample fp
        (NGOList fps) -> NGOList <$> sequence [NGOReadSet <$> doQC1 enc (T.unpack fname) | NGOString fname <- fps]
        v -> throwScriptError ("fastq function: unexpected first argument: " ++ show v)

executeQualityProcess :: NGLessObject -> NGLessIO NGLessObject
executeQualityProcess (NGOList e) = NGOList <$> mapM executeQualityProcess e
executeQualityProcess (NGOReadSet rs) = NGOReadSet <$> case rs of
    ReadSet1 enc fname -> doQC1 (Just enc) fname
    ReadSet2 enc fp1 fp2 -> do
        ReadSet1 enc1 fp1' <- doQC1 (Just enc) fp1
        ReadSet1 enc2 fp2' <- doQC1 (Just enc) fp2
        when (enc1 /= enc2) $
            throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ")")
        return (ReadSet2 enc1 fp1' fp2')
    ReadSet3 enc fp1 fp2 fp3 -> do
        ReadSet1 enc1 fp1' <- doQC1 (Just enc) fp1
        ReadSet1 enc2 fp2' <- doQC1 (Just enc) fp2
        ReadSet1 enc3 fp3' <- doQC1 (Just enc) fp3
        when (enc1 /= enc2 || enc2 /= enc3) $
            throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ", third one is " ++ show enc3 ++")")
        return (ReadSet3 enc1 fp1' fp2' fp3')
executeQualityProcess (NGOString fname) = NGOReadSet <$> doQC1 Nothing (T.unpack fname)
executeQualityProcess v = throwScriptError ("Sequence QC expected a string or readset. Got " ++ show v)
