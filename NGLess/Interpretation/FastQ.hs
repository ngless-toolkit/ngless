{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, MultiWayIf #-}

module Interpretation.FastQ
    ( executeFastq
    , executePaired
    , executeGroup
    , executeShortReadsMethod

    , encodingFor
    ) where

import System.IO
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Control.Concurrent.Async as A
import Control.Monad.Trans.Resource
import Control.Monad.Except
import Data.Conduit (($$), (=$=))
import Data.Maybe
import Data.Word

import FileManagement
import Data.FastQ
import Configuration
import Language
import Output
import Utils.Conduit
import NGLess
import NGLess.NGLEnvironment



-- | Guess the encoding of a file
encodingFor :: FilePath -> NGLessIO FastQEncoding
encodingFor fp = do
    let update :: Word8 -> Word8 -> B.ByteString -> (Word8, Word8)
        update minv maxv qs = (min minv $ B.minimum qs, max maxv $ B.maximum qs)
        encodingC minv maxv =
                C.await >>= \case
                    Nothing
                        | minv == 255 -> do
                            lift $ outputListLno' WarningOutput ["Input file ", fp, " is empty."]
                            return SangerEncoding -- It does not matter
                        | otherwise -> do
                            lift $ outputListLno' WarningOutput ["Heuristic for FastQ encoding determination for file ", show fp, " cannot be 100% confident. Guessing 33 offset (Sanger encoding, used by newer Illumina machines)."]
                            return SangerEncoding
                    Just [_, _, _, ByteLine qs] -> case update minv maxv qs of
                            (minv', maxv')
                                | minv' < 33 -> throwDataError ("No known encodings with chars < 33 (Yours was "++ show minv ++ ") for file '" ++ show fp ++ "'.")
                                | minv' < 58 -> return SangerEncoding
                                -- 33 + 45 should never happen with SangerEncoding, but it's quality 14 in SolexaEncoding, so should be common
                                | maxv' >= (33+45) -> return SolexaEncoding
                                | otherwise -> encodingC minv' maxv'
                    _ -> throwDataError ("Malformed file '" ++ fp ++ "': number of lines is not a multiple of 4.")


    C.runConduit $
        conduitPossiblyCompressedFile fp
        =$= linesCBounded
        =$= CL.chunksOf 4
        =$= encodingC 255 0

-- | Checks if file has no content
--
-- Note that this is more than checking if the file is empty: a compressed file
-- with no content will not be empty.
checkNoContent fp =
    conduitPossiblyCompressedFile fp
        =$= linesCBounded
        =$= CL.isolate 1
        $$ CL.fold (\_ _ -> False) True


-- | Drop every tenth FastQ group
drop10 = loop (0 :: Int)
    where
        loop 40 = loop 0
        loop !n = awaitJust $ \line -> do
                when (n < 4) (C.yield line)
                loop (n+1)


performSubsample :: FilePath -> Handle -> IO ()
performSubsample f h = do
    runResourceT $
            conduitPossiblyCompressedFile f
                =$= CB.lines
                =$= drop10
                =$= C.take 100000
                =$= C.unlinesAscii
                $$  asyncGzipTo h
    hClose h

optionalSubsample :: FilePath -> NGLessIO FilePath
optionalSubsample f = do
    subsampleActive <- nConfSubsample <$> nglConfiguration
    if subsampleActive
        then do
            outputListLno' TraceOutput ["Subsampling file ", f]
            (newfp,h) <- openNGLTempFile f "" "fq.gz"
            liftIO $ performSubsample f h
            outputListLno' TraceOutput ["Finished subsampling (temp sampled file is ", newfp, ")"]
            return newfp
        else return f


executeGroup :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeGroup (NGOList rs) args = do
        name <- lookupStringOrScriptError "group call" "name" args
        case rs of
            [r] -> addName name r
            _ -> do
                rs' <- getRSOrError `mapM` rs
                grouped <- runNGLess $ groupFiles name rs'
                return (NGOReadSet name grouped)
    where
        getRSOrError (NGOReadSet _ r) = return r
        getRSOrError other = throwShouldNotOccur . concat $ ["In group call, all arguments should have been NGOReadSet! Got ", show other]
        addName name (NGOReadSet _ r) = return $! NGOReadSet name r
        addName _  other = throwScriptError . concat $ ["In group call, all arguments should have been NGOReadSet, got ", show other]
executeGroup other _ = throwScriptError ("Illegal argument to group(): " ++ show other)

groupFiles :: T.Text -> [ReadSet] -> NGLess ReadSet
groupFiles context [] =
    throwDataError ("Attempted to group sample '" ++ T.unpack context ++ "' but sample is empty (no read files).")
groupFiles _ rs = return $! ReadSet (concatMap pairedSamples rs) (concatMap singleSamples rs)


executeFastq :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeFastq expr args = do
    enc <- getEncArgument "fastq" args
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "fastq hidden QC argument" "__perform_qc" args
    case expr of
        (NGOString fname) -> do
            fname' <- optionalSubsample (T.unpack fname)
            fq <- asFQFilePathMayQC qcNeeded enc fname'
            return $ NGOReadSet fname (ReadSet [] [fq])
        (NGOList fps) -> NGOList <$> sequence [NGOReadSet fname . ReadSet [] . (:[]) <$> asFQFilePathMayQC True enc (T.unpack fname) | NGOString fname <- fps]
        v -> throwScriptError ("fastq function: unexpected first argument: " ++ show v)


asFQFilePathMayQC :: Bool -- ^ whether to perform QC
                -> Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO FastQFilePath
asFQFilePathMayQC qc enc fp =  do
    enc' <- maybe (encodingFor fp) return enc
    when qc $ do
        s <- statsFromFastQ fp enc'
        outputFQStatistics fp s enc'
    return $! FastQFilePath enc' fp

executePaired :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executePaired (NGOString mate1) args = NGOReadSet mate1 <$> do
    enc <- getEncArgument "paired" args
    mate2 <- lookupStringOrScriptError "automatic argument" "second" args
    mate3 <- lookupStringOrScriptErrorDef (return "") "paired" "singles" args
    outputListLno' TraceOutput ["Executing paired on ", show mate1, show mate2, show mate3]
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "fastq hidden QC argument" "__perform_qc" args

    subsampleActive <- nConfSubsample <$> nglConfiguration
    (fp1', fp2') <- if subsampleActive
        then do
            (fp1',h1') <- openNGLTempFile (T.unpack mate1) "subsampled" "1.fq.gz"
            (fp2',h2') <- openNGLTempFile (T.unpack mate2) "subsampled" "2.fq.gz"
            outputListLno' TraceOutput ["Subsampling paired files ", T.unpack mate1, " and ", T.unpack mate2, " (parallel processing)"]
            liftIO $ A.concurrently_
                (performSubsample (T.unpack mate1) h1')
                (performSubsample (T.unpack mate2) h2')
            return (fp1', fp2')
        else return (T.unpack mate1, T.unpack mate2)
    pair@(FastQFilePath enc1 fp1, FastQFilePath enc2 fp2) <-
        if not qcNeeded
            then (,)
                 <$> asFQFilePathMayQC qcNeeded enc fp1'
                 <*> asFQFilePathMayQC qcNeeded enc fp2'
            else do
                enc1 <- fromMaybe (encodingFor fp1') (return <$> enc)
                enc2 <- fromMaybe (encodingFor fp1') (return <$> enc)
                (es1,es2) <- liftIO $ A.concurrently
                            (runExceptT $ statsFromFastQ fp1' enc1)
                            (runExceptT $ statsFromFastQ fp2' enc2)
                s1 <- runNGLess es1
                s2 <- runNGLess es2
                outputFQStatistics fp1' s1 enc1
                outputFQStatistics fp2' s2 enc2
                return (FastQFilePath enc1 fp1', FastQFilePath enc2 fp2')
    when (enc1 /= enc2) $
        throwDataError ("Mates do not seem to have the same quality encoding! (first mate [" ++ fp1 ++ "] had " ++ show enc1 ++ ", second one [" ++ fp2 ++ "] " ++ show enc2 ++ ").")
    case mate3 of
        "" -> return $! ReadSet [pair] []
        f3 -> do
            single@(FastQFilePath enc3 fp3) <- optionalSubsample (T.unpack f3) >>= asFQFilePathMayQC qcNeeded enc
            if (enc1 /= enc3)
                then do
                    is3empty <- checkNoContent fp3 -- this is a special case, but seen in the wild
                    unless is3empty $
                        throwDataError ("Mates do not seem to have the same quality encoding! (paired mates [" ++ fp1 ++ " had " ++ show enc1 ++ ", single one [" ++ fp3 ++ "] " ++ show enc3 ++ ").")
                    return $! ReadSet [pair] []
            else return $! ReadSet [pair] [single]
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

executeShortReadsMethod (MethodName "avg_quality") (ShortRead _ _ rQ) Nothing _ = return $! NGODouble $ fromIntegral (VS.foldl' (\acc n -> acc + toInteger n) (0 :: Integer) rQ) / fromIntegral (VS.length rQ)
executeShortReadsMethod (MethodName "fraction_at_least") (ShortRead _ _ rQ) (Just (NGOInteger minq)) _ = return $! NGODouble $ fromIntegral (VS.foldl' (\acc q -> acc + fromEnum (q >= fromInteger minq)) (0 :: Int) rQ) / fromIntegral (VS.length rQ)
executeShortReadsMethod (MethodName "n_to_zero_quality") (ShortRead h sq rQ) Nothing _ = return . NGOShortRead . ShortRead h sq $ VS.generate (VS.length rQ) (\ix ->
                                                                                                                            if B8.index sq ix == 'N' || B8.index sq ix == 'n'
                                                                                                                                then 0
                                                                                                                                else rQ VS.! ix)
executeShortReadsMethod (MethodName other) _ _ _ = throwShouldNotOccur ("Unknown short read method: " ++ show other)
