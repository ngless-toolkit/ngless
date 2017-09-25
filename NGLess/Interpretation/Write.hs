{- Copyright 2013-2017 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts #-}

module Interpretation.Write
    ( executeWrite
    , _formatFQOname
    ) where


import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as C
import           Data.Conduit ((=$=), runConduit, ($$), (.|))
import           System.Directory (copyFile)
import           System.IO (hClose)
import Data.String.Utils
import Data.List (isInfixOf)

import Data.FastQ
import Language
import Configuration
import FileOrStream
import FileManagement (openNGLTempFile)
import NGLess
import Output
import Utils.Utils
import NGLess.NGLEnvironment
import Utils.Samtools (convertSamToBam)
import Utils.Conduit

{- A few notes:
    There is a transform pass which adds the argument __can_move to write() calls.
    If canMove is True, then we can move the input instead of copying as it
    will no longer be used in the script.

    Decisions on whether to use compression are based on the filenames.

    The filepath "/dev/stdout" is special cased to print to stdout
-}

moveOrCopyCompress :: Bool -> FilePath -> FilePath -> NGLessIO ()
moveOrCopyCompress _ orig "/dev/stdout" = conduitPossiblyCompressedFile orig $$ C.stdout
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
                       else maybeCopyFile

        liftIO2 f a b = liftIO (f a b)
        isGZ = endswith ".gz"
        igz = isGZ orig
        ogz = isGZ fname
        uncompressTo oldfp newfp = runConduit $
            conduitPossiblyCompressedFile oldfp =$= C.sinkFile newfp
        compressTo oldfp newfp = runConduit $
            C.sourceFile oldfp =$= asyncGzipToFile newfp

        -- | copy file unless its the same file.
        maybeCopyFile :: FilePath -> FilePath -> NGLessIO ()
        maybeCopyFile old new
            | new == old = return()
            | otherwise = liftIO (copyFile old new)

removeEnd :: String -> String -> String
removeEnd base suffix = take (length base - length suffix) base

_formatFQOname :: FilePath -> FilePath -> NGLessIO FilePath
_formatFQOname base insert
    | base `isInfixOf` "{index}" = return (replace base "{index}" ("." ++ insert ++ "."))
    | endswith ".fq" base = return $ removeEnd base ".fq" ++ "." ++ insert ++ ".fq"
    | endswith ".fq.gz" base = return $ removeEnd base ".fq" ++ "." ++ insert ++ ".fq.gz"
    | otherwise = throwScriptError ("Cannot handle filename " ++ base ++ " (expected extension .fq/.fq.gz/.fq.bz2).")

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
    let moveOrCopyCompressFQs :: [FastQFilePath] -> FilePath -> NGLessIO ()
        moveOrCopyCompressFQs [] _ = return ()
        moveOrCopyCompressFQs [FastQFilePath _ f] ofname = moveOrCopyCompress canMove f ofname
        moveOrCopyCompressFQs multiple ofname = do
            let inputs = fqpathFilePath <$> multiple
            (fp',h) <- openNGLTempFile (head inputs) "concat" "tmp"
            C.runConduit
                (mapM_ C.sourceFile inputs .| C.sinkHandle h)
            liftIO $ hClose h
            moveOrCopyCompress True fp' ofname


    case rs of
        ReadSet [] singles -> do
            moveOrCopyCompressFQs singles ofile
            return NGOVoid
        ReadSet pairs [] -> do
            fname1 <- _formatFQOname ofile "pair.1"
            fname2 <- _formatFQOname ofile "pair.2"
            moveOrCopyCompressFQs (fst <$> pairs) fname1
            moveOrCopyCompressFQs (snd <$> pairs) fname2
            return NGOVoid
        ReadSet pairs singletons -> do
            fname1 <- _formatFQOname ofile "pair.1"
            fname2 <- _formatFQOname ofile "pair.2"
            fname3 <- _formatFQOname ofile "singles"
            moveOrCopyCompressFQs (fst <$> pairs) fname1
            moveOrCopyCompressFQs (snd <$> pairs) fname2
            moveOrCopyCompressFQs singletons fname3
            return NGOVoid
executeWrite el@(NGOMappedReadSet _ iout  _) args = do
    fp <- asFile iout
    newfp <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    let guess :: String -> T.Text
        guess "/dev/stdout" = "sam"
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

executeWrite (NGOCounts iout) args = do
    newfp <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    format <- lookupSymbolOrScriptErrorDef (return "tsv") "internal write arg" "format" args
    outputListLno' InfoOutput ["Writing counts to: ", newfp]
    case format of
        "tsv" -> do
            fp <- asFile iout
            moveOrCopyCompress canMove fp newfp
        "csv" -> do
            let (fp,istream) = asStream iout
            (comma,ohand) <- openNGLTempFile fp "wcomma" "csv"
            runConduit $
                istream =$= CL.map tabToComma =$= byteLineSinkHandle ohand
            liftIO $ hClose ohand
            moveOrCopyCompress True comma newfp
        _ -> throwScriptError ("Invalid format in write: {"++T.unpack format++"}.\n\tWhen writing counts, only accepted values are {tsv} (TAB separated values; default) or {csv} (COMMA separated values).")
    return NGOVoid
  where
    tabToComma :: ByteLine -> ByteLine
    tabToComma (ByteLine line) = ByteLine $ B8.map (\case { '\t' -> ','; c -> c }) line

executeWrite (NGOFilename fp) args = do
    newfp <- getOFile args
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    moveOrCopyCompress canMove fp newfp
    return NGOVoid

executeWrite v _ = throwShouldNotOccur ("Error: executeWrite of " ++ show v ++ " not implemented yet.")


