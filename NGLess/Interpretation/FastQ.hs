{- Copyright 2013-2017 NGLess Authors
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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
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
import Utils.Utils
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
        =$= groupC 4
        =$= encodingC 255 0

-- | Checks if file has no content
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

-- ^ Process quality.
doQC1 :: Maybe FastQEncoding -- ^ encoding to use (or autodetect)
                -> FilePath         -- ^ FastQ file
                -> NGLessIO ReadSet
doQC1 enc f = do
        enc' <- maybe (encodingFor f) return enc
        fd <- statsFromFastQ f enc'
        outputFQStatistics f fd enc'
        return (ReadSet1 enc' f)

executeGroup :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeGroup (NGOList rs) args = do
        name <- lookupStringOrScriptError "group call" "name" args
        case rs of
            [r] -> addName name r
            _ -> do
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
groupFiles name [] =
    throwDataError ("Attempted to group sample '" ++ T.unpack name ++ "' but sample is empty (no read files).")
groupFiles name rs = do
    let encs = map rsEncoding rs
    unless (allSame encs) $
        throwDataError "In group call not all input files have the same encoding!"
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

catFiles3 _ [] = throwShouldNotOccur "catFiles3 called with an empty list"
catFiles3 name rs@(r:_) = do
    (newfp1, h1) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.1.fq.gz"
    (newfp2, h2) <- openNGLTempFile (T.unpack name) "concatenated_" ".paired.2.fq.gz"
    (newfp3, h3) <- openNGLTempFile (T.unpack name) "concatenated_" ".singles.fq.gz"
    let enc = rsEncoding r
    forM_ rs $ \case
        ReadSet1 _ fp -> hCat h3 fp
        ReadSet2 _ fp1 fp2 -> do
            outputListLno' TraceOutput ["Concatenating ", fp1, " # ", fp2]
            hCat h1 fp1
            hCat h2 fp2
        ReadSet3 _ fp1 fp2 fp3 -> do
            outputListLno' TraceOutput ["Concatenating ", fp1, " # ", fp2, " # ", fp3]
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
        (NGOString fname) -> NGOReadSet fname <$> do
            fname' <- optionalSubsample (T.unpack fname)
            asReadSet1mayQC qcNeeded enc fname'
        (NGOList fps) -> NGOList <$> sequence [NGOReadSet fname <$> doQC1 enc (T.unpack fname) | NGOString fname <- fps]
        v -> throwScriptError ("fastq function: unexpected first argument: " ++ show v)


asReadSet1mayQC :: Bool -> Maybe FastQEncoding -> FilePath -> NGLessIO ReadSet
asReadSet1mayQC True enc fp = doQC1 enc fp
asReadSet1mayQC False enc fp = do
    enc' <- fromMaybe (encodingFor fp) (return <$> enc)
    return (ReadSet1 enc' fp)

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
            void . liftIO $ A.concurrently
                (performSubsample (T.unpack mate1) h1')
                (performSubsample (T.unpack mate2) h2')
            return (fp1', fp2')
        else return (T.unpack mate1, T.unpack mate2)
    (ReadSet1 enc1 fp1) <- asReadSet1mayQC qcNeeded enc fp1'
    (ReadSet1 enc2 fp2) <- asReadSet1mayQC qcNeeded enc fp2'
    when (enc1 /= enc2) $
        throwDataError ("Mates do not seem to have the same quality encoding! (first mate [" ++ fp1 ++ "] had " ++ show enc1 ++ ", second one [" ++ fp2 ++ "] " ++ show enc2 ++ ").")
    case mate3 of
        "" -> return (ReadSet2 enc1 fp1 fp2)
        f3 -> do
            (ReadSet1 enc3 fp3) <- optionalSubsample (T.unpack f3) >>= asReadSet1mayQC qcNeeded enc
            if (enc1 /= enc3)
                then do
                    is3empty <- checkNoContent fp3 -- this is a special case, but seen in the wild
                    unless is3empty $
                        throwDataError ("Mates do not seem to have the same quality encoding! (paired mates [" ++ fp1 ++ " had " ++ show enc1 ++ ", single one [" ++ fp3 ++ "] " ++ show enc3 ++ ").")
                    return $! ReadSet2 enc1 fp1 fp2
            else return $! ReadSet3 enc1 fp1 fp2 fp3
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

executeShortReadsMethod (MethodName "avg_quality") (ShortRead _ _ rQ) Nothing _ = return $! NGODouble $ fromIntegral (VU.foldl' (\acc n -> acc + toInteger n) (0 :: Integer) rQ) / fromIntegral (VU.length rQ)
executeShortReadsMethod (MethodName "fraction_at_least") (ShortRead _ _ rQ) (Just (NGOInteger minq)) _ = return $! NGODouble $ fromIntegral (VU.foldl' (\acc q -> acc + fromEnum (q >= fromInteger minq)) (0 :: Int) rQ) / fromIntegral (VU.length rQ)
executeShortReadsMethod (MethodName other) _ _ _ = throwShouldNotOccur ("Unknown short read method: " ++ show other)
