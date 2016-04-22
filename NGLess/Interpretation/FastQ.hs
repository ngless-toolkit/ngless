{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiWayIf #-}

module Interpretation.FastQ
    ( executeFastq
    , executePaired
    , executeGroup
    ) where

import System.IO
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($$), (=$=))
import Data.Maybe

import FileManagement
import Data.FastQ
import Configuration
import Language
import Output
import Utils.Conduit
import Utils.Utils
import NGLess

drop100 = loop (0 :: Int)
    where
        loop 400 = loop 0
        loop !n = awaitJust $ \line -> do
                when (n < 4) (C.yield line)
                loop (n+1)

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
                $$  asyncGzipTo h
            liftIO $ hClose h
            outputListLno' TraceOutput ["Finished subsampling (temp sampled file is ", newfp, ")"]
            return newfp

-- ^ Process quality.
doQC1 :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO ReadSet
doQC1 enc f = do
        fd <- statsFromFastQ f
        enc' <- case enc of
                Just e -> return e
                Nothing -> guessEncoding (lc fd)
        outputFQStatistics f fd enc'
        return (ReadSet1 enc' f)

executeGroup :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeGroup (NGOList rs) args = do
        name <- lookupStringOrScriptError "group call" "name" args
        if length rs == 1
            then addName name (rs !! 0)
            else do
                rs' <- getRSOrError `mapM` rs
                grouped <- groupFiles name rs'
                return (NGOReadSet name grouped)
    where
        getRSOrError (NGOReadSet _ r) = return r
        getRSOrError other = throwShouldNotOccur . concat $ ["In group call, all arguments should have been NGOReadSet! Got ", show other]
        addName name (NGOReadSet _ r) = return $! NGOReadSet name r
        addName _  other = throwScriptError . concat $ ["In group call, all arguments should have been NGOReadSet, got ", show other]
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
    (newfp, h) <- openNGLTempFile (T.unpack name) "concatenated_" "fq.gz"
    forM_ rs $ \(ReadSet1 _ fp) ->
        hCat h fp
    liftIO (hClose h)
    return (ReadSet1 enc newfp)
catFiles1 _ rs = throwShouldNotOccur ("catFiles1 called with args : " ++ show rs)

catFiles2 name rs@(ReadSet2 enc _ _:_) = do
    (newfp1, h1) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.1.fq.gz"
    (newfp2, h2) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.2.fq.gz"
    forM_ rs $ \(ReadSet2 _ fp1 fp2) -> do
        hCat h1 fp1
        hCat h2 fp2
    liftIO (hClose h1)
    liftIO (hClose h2)
    return (ReadSet2 enc newfp1 newfp2)
catFiles2 _ rs = throwShouldNotOccur ("catFiles2 called with args : " ++ show rs)

catFiles3 _ [] = throwShouldNotOccur ("catFiles3 called with an empty list" :: String)
catFiles3 name rs@(r:_) = do
    (newfp1, h1) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.1.fq.gz"
    (newfp2, h2) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.2.fq.gz"
    (newfp3, h3) <- openNGLTempFile (T.unpack name) "concatenated_" ".singles.fq.gz"
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

hCat h fp = CB.sourceFile fp $$ CB.sinkHandle h
rsEncoding (ReadSet1 enc _) = enc
rsEncoding (ReadSet2 enc _ _) = enc
rsEncoding (ReadSet3 enc _ _ _) = enc


executeFastq :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeFastq expr args = do
    enc <- getEncArgument "fastq" args
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "fastq hidden QC argument" "__perform_qc" args
    case expr of
        (NGOString fname) -> NGOReadSet fname <$> asReadSet1mayQC qcNeeded enc fname
        (NGOList fps) -> NGOList <$> sequence [NGOReadSet fname <$> doQC1 enc (T.unpack fname) | NGOString fname <- fps]
        v -> throwScriptError ("fastq function: unexpected first argument: " ++ show v)


asReadSet1mayQC :: Bool -> Maybe FastQEncoding -> T.Text -> NGLessIO ReadSet
asReadSet1mayQC qcNeeded enc fpt = do
    let fp = T.unpack fpt
    fp' <- optionalSubsample fp
    if qcNeeded
        then doQC1 enc fp'
        else do
            enc' <- fromMaybe (encodingFor fp') (return <$> enc)
            return (ReadSet1 enc' fp')

executePaired :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executePaired (NGOString mate1) args = NGOReadSet mate1 <$> do
    enc <- getEncArgument "paired" args
    mate2 <- lookupStringOrScriptError "automatic argument" "second" args
    let mate3 = lookup "singles" args
    outputListLno' TraceOutput ["Executing paired on ", show mate1, show mate2, show mate3]
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "fastq hidden QC argument" "__perform_qc" args

    (ReadSet1 enc1 fp1) <- asReadSet1mayQC qcNeeded enc mate1
    (ReadSet1 enc2 fp2) <- asReadSet1mayQC qcNeeded enc mate2
    when (enc1 /= enc2) $
        throwDataError ("Mates do not seem to have the same quality encoding!" :: String)
    case mate3 of
        Nothing -> return (ReadSet2 enc1 fp1 fp2)
        Just (NGOString f3) -> do
            (ReadSet1 enc3 fp3) <- asReadSet1mayQC qcNeeded enc f3
            when (enc1 /= enc3) $
                throwDataError ("Mates do not seem to have the same quality encoding!" :: String)
            return (ReadSet3 enc1 fp1 fp2 fp3)
        Just other -> throwScriptError ("Function paired expects a string for argument 'singles', got " ++ show other)
executePaired expr _ = throwScriptError ("Function paired expects a string, got: " ++ show expr)

getEncArgument fname args =
    lookupSymbolOrScriptErrorDef (return "auto") fname "encoding" args
        >>= \case
            "auto" -> return Nothing
            "33" -> return $ Just SangerEncoding
            "sanger" -> return $ Just SangerEncoding
            "64" -> return $ Just SolexaEncoding
            "solexa" -> return $ Just SolexaEncoding
            other -> throwScriptError ("Unkown encoding for fastq " ++ T.unpack other)
