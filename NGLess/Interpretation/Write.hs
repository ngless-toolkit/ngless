{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Write
    ( executeWrite
    , _formatFQOname
    ) where


import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C
import           Data.Conduit ((=$=), runConduit)
import Data.String.Utils
import Data.List (isInfixOf)

import Language
import FileManagement
import Configuration
import FileOrStream
import NGLess
import Output
import Utils.Utils
import Utils.Samtools (convertSamToBam)
import Utils.Conduit (conduitPossiblyCompressedFile, asyncGzipToFile)

{- A few notes:
    There is a transform pass which adds the argument __can_move to write() calls.
    If canMove is True, then we can move the input instead of copying as it
    will no longer be used in the script.

    Decisions on whether to use compression are based on the filenames.
-}

moveOrCopyCompress :: Bool -> FilePath -> FilePath -> NGLessIO ()
moveOrCopyCompress canMove orig fname = moveOrCopyCompress' orig fname
    where
        moveOrCopyCompress' :: FilePath -> FilePath -> NGLessIO ()
        moveOrCopyCompress'
            | igz && ogz = moveIfCan
            | igz = uncompressTo
            | ogz = compressTo
            | otherwise = moveIfCan

        moveIfCan :: FilePath -> FilePath -> NGLessIO ()
        moveIfCan = if canMove
                       then liftIO2 moveOrCopy
                       else nglMaybeCopyFile

        liftIO2 f = \a b -> liftIO (f a b)
        isGZ = endswith ".gz"
        igz = isGZ orig
        ogz = isGZ fname
        uncompressTo oldfp newfp = runConduit $
            conduitPossiblyCompressedFile oldfp =$= C.sinkFile newfp
        compressTo oldfp newfp = runConduit $
            C.sourceFile oldfp =$= asyncGzipToFile newfp

removeEnd :: String -> String -> String
removeEnd base suffix = take (length base - length suffix) base

_formatFQOname :: FilePath -> FilePath -> NGLessIO FilePath
_formatFQOname base insert
    | base `isInfixOf` "{index}" = return (replace base "{index}" ("." ++ insert ++ "."))
    | endswith ".fq" base = return $ removeEnd base ".fq" ++ "." ++ insert ++ ".fq"
    | endswith ".fq.gz" base = return $ removeEnd base ".fq" ++ "." ++ insert ++ ".fq.gz"
    | otherwise = throwScriptError ("Cannot handle " ++ base)

getOFile :: KwArgsValues -> NGLessIO FilePath
getOFile args = do
    sub <- nConfSubsample <$> nglConfiguration
    let subpostfix = if sub then ".subsampled" else ""
    case lookup "ofile" args of
        Just (NGOFilename p) -> return (p ++ subpostfix)
        Just (NGOString p) -> return (T.unpack p ++ subpostfix)
        _ -> throwShouldNotOccur "getOFile cannot decode file path"


executeWrite :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeWrite (NGOList el) args = do
    templateFP <- getOFile args
    let args' = filter (\(a,_) -> (a /= "ofile")) args
        fps = map ((\fname -> replace "{index}" fname templateFP) . show) [1..length el]
    zipWithM_ (\e fp -> executeWrite e (("ofile", NGOFilename fp):args')) el fps
    return NGOVoid

executeWrite (NGOReadSet _ rs) args = do
    ofile <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    case rs of
        ReadSet1 _ r -> do
            moveOrCopyCompress canMove r ofile
            return NGOVoid
        ReadSet2 _ r1 r2 -> do
            fname1 <- _formatFQOname ofile "pair.1"
            fname2 <- _formatFQOname ofile "pair.2"
            moveOrCopyCompress canMove r1 fname1
            moveOrCopyCompress canMove r2 fname2
            return NGOVoid
        ReadSet3 _ r1 r2 r3 -> do
            fname1 <- _formatFQOname ofile "pair.1"
            fname2 <- _formatFQOname ofile "pair.2"
            fname3 <- _formatFQOname ofile "singles"
            moveOrCopyCompress canMove r1 fname1
            moveOrCopyCompress canMove r2 fname2
            moveOrCopyCompress canMove r3 fname3
            return NGOVoid
executeWrite el@(NGOMappedReadSet _ (File fp) _) args = do
    newfp <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    let guess :: String -> T.Text
        guess ofile
            | endswith ".sam" ofile = "sam"
            | endswith ".bam" ofile = "bam"
            | otherwise = "bam"
    format <- lookupSymbolOrScriptErrorDef (return $ guess newfp) "format for mappedreadset" "format" args
    orig <- case format of
        "sam" -> return fp
        "bam"
            | endswith ".bam" fp -> return fp -- We already have a BAM, so just copy it
            | otherwise -> convertSamToBam fp
        s -> throwScriptError ("write does not accept format {" ++ T.unpack s ++ "} with input type " ++ show el)
    moveOrCopyCompress canMove orig newfp
    return NGOVoid

executeWrite (NGOCounts (File fp)) args = do
    newfp <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    outputListLno' InfoOutput ["Writing counts to: ", newfp]
    moveOrCopyCompress canMove fp newfp
    return NGOVoid

executeWrite v _ = throwShouldNotOccur ("Error: executeWrite of " ++ show v ++ " not implemented yet.")

getDelimiter :: NGLessObject -> NGLessIO B.ByteString
getDelimiter (NGOSymbol "csv") = return ","
getDelimiter (NGOSymbol "tsv") = return "\t"
getDelimiter (NGOSymbol f) = throwScriptError ("Invalid format in write: {"++T.unpack f++"}")
getDelimiter v =  throwShouldNotOccur ("Type of 'format' in 'write' must be NGOSymbol, got " ++ show v)

