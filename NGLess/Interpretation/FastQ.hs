{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiWayIf #-}

module Interpretation.FastQ
    ( executeFastq
    , executeGroup
    , executeQualityProcess
    , optionalSubsample
    , writeTempFastQ
    , _doQC1
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
import Utils.Conduit (conduitPossiblyCompressedFile)
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
            conduitPossiblyCompressedFile f
                =$= CB.lines
                =$= drop100
                =$= C.unlinesAscii
                =$= C.gzip
                $$ CB.sinkHandle h
            liftIO $ hClose h
            outputListLno' TraceOutput ["Finished subsampling (temp sampled file is ", newfp, ")"]
            return newfp

-- ^ Process quality.
_doQC1 :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO ReadSet
_doQC1 enc f = do
        fd <- liftIO $ statsFromFastQ <$> readPossiblyCompressedFile f
        enc' <- case enc of
                Just e -> return e
                Nothing -> guessEncoding (lc fd)
        outputFQStatistics f fd enc'
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
        grouped <- groupFiles name rs'
        return (NGOSample name [grouped])
    where
        getRSOrError (NGOReadSet r) = return r
        getRSOrError other = throwShouldNotOccur . concat $ ["In group call, all arguments should have been NGOReadSet! Got ", show other]
executeGroup other _ = throwScriptError ("Illegal argument to group(): " ++ show other)

groupFiles :: T.Text -> [ReadSet] -> NGLessIO ReadSet
groupFiles name rs = do
    let encs = map rsEncoding rs
    unless (allSame encs) $
        throwDataError ("In group call not all input files have the same encoding!" :: String)
    let dims = map dim1 rs
        dim1 :: ReadSet -> Int
        dim1 ReadSet1{} = 1
        dim1 ReadSet2{} = 2
        dim1 ReadSet3{} = 3
    if
        | all (==1) dims -> catFiles1 name rs
        | all (==2) dims -> catFiles2 name rs
        -- If it has a single 3 or a mix of 2s & 1s, then it's a 3
        | otherwise -> catFiles3 name rs


catFiles1 name rs@(ReadSet1 enc _:_) = do
    (newfp, h) <- openNGLTempFile (T.unpack name) "concatenated_" "fq"
    forM_ rs $ \(ReadSet1 _ fp) ->
        hCat h fp
    liftIO (hClose h)
    return (ReadSet1 enc newfp)
catFiles1 _ rs = throwShouldNotOccur ("catFiles1 called with args : " ++ show rs)

catFiles2 name rs@(ReadSet2 enc _ _:_) = do
    (newfp1, h1) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.1.fq"
    (newfp2, h2) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.2.fq"
    forM_ rs $ \(ReadSet2 _ fp1 fp2) -> do
        hCat h1 fp1
        hCat h2 fp2
    liftIO (hClose h1)
    liftIO (hClose h2)
    return (ReadSet2 enc newfp1 newfp2)
catFiles2 _ rs = throwShouldNotOccur ("catFiles2 called with args : " ++ show rs)

catFiles3 name rs@(r:_) = do
    (newfp1, h1) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.1.fq"
    (newfp2, h2) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.2.fq"
    (newfp3, h3) <- openNGLTempFile (T.unpack name) "concatenated_" ".singles.fq"
    let enc = rsEncoding r
    forM_ rs $ \case
        ReadSet1 _ fp -> hCat h3 fp
        ReadSet2 _ fp1 fp2 -> do
            hCat h1 fp1
            hCat h2 fp2
        ReadSet3 _ fp1 fp2 fp3 -> do
            hCat h1 fp1
            hCat h2 fp2
            hCat h3 fp3
    liftIO (hClose h1)
    liftIO (hClose h2)
    liftIO (hClose h3)
    return (ReadSet3 enc newfp1 newfp2 newfp3)

hCat h fp = liftIO (BL.readFile fp >>= BL.hPut h)
rsEncoding (ReadSet1 enc _) = enc
rsEncoding (ReadSet2 enc _ _) = enc
rsEncoding (ReadSet3 enc _ _ _) = enc


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
            _doQC1 enc =<< optionalSubsample fp
        (NGOList fps) -> NGOList <$> sequence [NGOReadSet <$> _doQC1 enc (T.unpack fname) | NGOString fname <- fps]
        v -> throwScriptError ("fastq function: unexpected first argument: " ++ show v)

executeQualityProcess :: NGLessObject -> NGLessIO NGLessObject
executeQualityProcess (NGOList e) = NGOList <$> mapM executeQualityProcess e
executeQualityProcess (NGOReadSet rs) = NGOReadSet <$> case rs of
    ReadSet1 enc fname -> _doQC1 (Just enc) fname
    ReadSet2 enc fp1 fp2 -> do
        ReadSet1 enc1 fp1' <- _doQC1 (Just enc) fp1
        ReadSet1 enc2 fp2' <- _doQC1 (Just enc) fp2
        when (enc1 /= enc2) $
            throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ")")
        return (ReadSet2 enc1 fp1' fp2')
    ReadSet3 enc fp1 fp2 fp3 -> do
        ReadSet1 enc1 fp1' <- _doQC1 (Just enc) fp1
        ReadSet1 enc2 fp2' <- _doQC1 (Just enc) fp2
        ReadSet1 enc3 fp3' <- _doQC1 (Just enc) fp3
        when (enc1 /= enc2 || enc2 /= enc3) $
            throwDataError ("Mates do not seem to have the same quality encoding! (first one is " ++ show enc1++" while second one is "++show enc2 ++ ", third one is " ++ show enc3 ++")")
        return (ReadSet3 enc1 fp1' fp2' fp3')
executeQualityProcess (NGOString fname) = NGOReadSet <$> _doQC1 Nothing (T.unpack fname)
executeQualityProcess v = throwScriptError ("Sequence QC expected a string or readset. Got " ++ show v)
