{- Copyright 2013-2022 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts, CPP #-}

module Interpretation.Write
    ( executeWrite
    , moveOrCopyCompress
    , WriteOptions(..)
#ifdef IS_BUILDING_TEST
    , _formatFQOname
#endif
    ) where



import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import qualified Data.Conduit.Algorithms.Async as CAsync
import           Data.Conduit ((.|))
import           System.Directory (copyFile)
import           Data.Default (Default(..))
import           Data.Maybe
import           Data.String.Utils (replace, endswith)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Except
import           Control.Monad.Catch (MonadMask)
import           System.IO (Handle, stdout)
import           Data.List (isInfixOf)
import           Control.Concurrent.Async (concurrently_)

import Data.FastQ
import Language
import Configuration
import FileOrStream
import FileManagement (makeNGLTempFile, inferCompression, Compression(..))
import NGLess
import Output
import NGLess.NGLEnvironment
import Utils.Samtools (convertSamToBam, convertBamToSam)
import Utils.Conduit
import Utils.Utils (withOutputFile, fmapMaybeM, moveOrCopy)

{- A few notes:
    There is a transform pass which adds the argument __can_move to write() calls.
    If canMove is True, then we can move the input instead of copying as it
    will no longer be used in the script.

    Decisions on whether to use compression are based on the filenames.

    The filepath "/dev/stdout" is special cased to print to stdout
-}

data WriteOptions = WriteOptions
                { woOFile :: FilePath
                , woFormat :: Maybe T.Text
                , woFormatFlags :: Maybe T.Text
                , woCanMove :: Bool
                , woVerbose :: Bool
                , woComment :: Maybe T.Text
                , woAutoComment :: [AutoComment]
                , woHash :: T.Text
                , woCompressLevel :: Maybe Int
                } deriving (Eq)

instance Default WriteOptions where
    def = WriteOptions
            { woOFile= ""
            , woFormat = Nothing
            , woFormatFlags = Nothing
            , woCanMove = False
            , woVerbose = False
            , woComment = Nothing
            , woAutoComment = []
            , woHash = ""
            , woCompressLevel = Nothing
            }


-- The type is tricky because the inner monad (m') need not be the same as the outer monad (m)
-- (e.g., the inner monad may be (C.ResourceT m) while the outer monad is m)
withOutputFileO :: (MonadUnliftIO m, MonadMask m, MonadUnliftIO m', C.MonadResource m')
            => WriteOptions -> (C.ConduitT B.ByteString C.Void m' () -> m a) -> m a
withOutputFileO wo f =
        withOutputFile' (woOFile wo) $ \hout ->
            f $ ostream (woOFile wo) hout
    where
        withOutputFile' :: (MonadUnliftIO m, MonadMask m) => FilePath -> (Handle -> m a) -> m a
        withOutputFile' "/dev/stdout" = \inner -> inner stdout
        withOutputFile' fname = withOutputFile fname

        comp :: Maybe Int
        comp = woCompressLevel wo

        ostream :: (MonadUnliftIO m, C.MonadResource m) => FilePath -> Handle -> C.ConduitT B.ByteString C.Void m ()
        ostream fp = case inferCompression fp of
            NoCompression -> CB.sinkHandle
            GzipCompression -> maybe CAsync.asyncGzipTo CAsync.asyncGzipTo' comp
            BZ2Compression -> CAsync.asyncBzip2To
            XZCompression -> maybe CAsync.asyncXzTo CAsync.asyncXzTo' comp
            ZStdCompression -> CAsync.asyncZstdTo (fromMaybe 3 comp)

parseWriteOptions :: KwArgsValues -> NGLessIO WriteOptions
parseWriteOptions args = do
    sub <- nConfSubsample <$> nglConfiguration
    let subpostfix = if sub then ".subsampled" else ""
    ofile <- case lookup "ofile" args of
        Just (NGOFilename p) -> return (p ++ subpostfix)
        Just (NGOString p) -> return (T.unpack p ++ subpostfix)
        _ -> throwShouldNotOccur "getOFile cannot decode file path"
    format <- fmapMaybeM (symbolOrTypeError "format argument to write() function") (lookup "format" args)
    canMove <- lookupBoolOrScriptErrorDef (return False) "internal write arg" "__can_move" args
    verbose <- lookupBoolOrScriptErrorDef (return False) "write arg" "verbose" args
    comment <- fmapMaybeM (stringOrTypeError "comment argument to write() function") (lookup "comment" args)
    autoComments <- case lookup "auto_comments" args of
                        Nothing -> return []
                        Just (NGOList cs) -> mapM (\s -> do
                                                        let errmsg = "auto_comments argument in write() call"
                                                        symbolOrTypeError errmsg s >>=
                                                            decodeSymbolOrError errmsg
                                                                [("date", AutoDate)
                                                                ,("script", AutoScript)
                                                                ,("hash", AutoResultHash)]) cs
                        _ -> throwScriptError "auto_comments argument to write() call must be a list of symbols"
    hash <- lookupStringOrScriptError "hidden __hash argument to write() function" "__hash" args
    formatFlags <- case lookup "format_flags" args of
                        Nothing -> return Nothing
                        Just (NGOSymbol flag) -> return $ Just flag
                        Just other -> throwScriptError $ "format_flags argument to write(): illegal argument ("++show other++")"
    compressLevel <- case lookup "compress_level" args of
                        Nothing -> return Nothing
                        Just (NGOInteger level) -> return . Just $ fromEnum level
                        Just other -> throwScriptError $ "compress_level argument to write(): illegal argument ("++show other++")"
    return $! WriteOptions
                { woOFile = ofile
                , woFormat = format
                , woFormatFlags = formatFlags
                , woCanMove = canMove
                , woVerbose = verbose
                , woComment = comment
                , woAutoComment = autoComments
                , woHash = hash
                , woCompressLevel = compressLevel
                }


moveOrCopyCompress :: WriteOptions -> FilePath -> NGLessIO ()
moveOrCopyCompress opts ifile = liftIO =<< moveOrCopyCompress' opts ifile

moveOrCopyCompress' :: WriteOptions -> FilePath -> NGLessIO (IO ())
moveOrCopyCompress' opts ifile
        | ofile == "/dev/stdout" = return (C.runConduitRes $ conduitPossiblyCompressedFile ifile .| C.stdoutC)
        | ofile == ifile = return (return ()) -- trivial case. Can happen.
#ifdef WINDOWS
        | ocompression == BZ2Compression = throwNotImplementedError "Compression of bzip2 files is not supported on Windows"
        | icompression == BZ2Compression = throwNotImplementedError "Decompression of bzip2 files is not supported on Windows"
#endif
        | icompression == ocompression = moveIfAllowed
        | otherwise = convertCompression
    where
        ofile = woOFile opts
        moveIfAllowed :: NGLessIO (IO ())
        moveIfAllowed = do
                createdFiles <- ngleTemporaryFilesCreated <$> nglEnvironment
                if woCanMove opts && ifile `elem` createdFiles
                    then return (moveOrCopy ifile ofile)
                    else return (copyFile ifile ofile)

        icompression = inferCompression ifile
        ocompression = inferCompression ofile

        convertCompression :: NGLessIO (IO ())
        convertCompression = return $
            withOutputFileO opts $ \out ->
                C.runConduitRes (conduitPossiblyCompressedFile ifile .| out)

removeEnd :: String -> String -> String
removeEnd base suffix = take (length base - length suffix) base

_formatFQOname :: MonadError NGError m => FilePath -> FilePath -> m FilePath
_formatFQOname base insert
    | endswith ".subsampled" base = _formatFQOname (take (length base - length (".subsampled" :: String)) base) (insert ++ ".subsampled")
    | "{index}" `isInfixOf` base = return $ replace "{index}" insert base
    | endswith ".fq" base = return $ removeEnd base ".fq" ++ "." ++ insert ++ ".fq"
    | endswith ".fq.gz" base = return $ removeEnd base ".fq.gz" ++ "." ++ insert ++ ".fq.gz"
    | endswith ".fq.bz2" base = return $ removeEnd base ".fq.bz2" ++ "." ++ insert ++ ".fq.bz2"
    | otherwise = throwScriptError ("Cannot handle filename " ++ base ++ " (expected extension .fq/.fq.gz/.fq.bz2).")



executeWrite :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeWrite (NGOList el) args = do
    templateFP <- woOFile <$> parseWriteOptions args
    let args' = filter (\(a,_) -> (a /= "ofile")) args
        fps = map ((\fname -> replace "{index}" fname templateFP) . show) [1..length el]
    zipWithM_ (\e fp -> executeWrite e (("ofile", NGOFilename fp):args')) el fps
    return (NGOFilename templateFP)

executeWrite (NGOReadSet _ rs) args = do
    opts <- parseWriteOptions args
    let ofile = woOFile opts
        moveOrCopyCompressFQs :: [FastQFilePath] -> FilePath -> NGLessIO (IO ())
        moveOrCopyCompressFQs [] _ = return (return ())
        moveOrCopyCompressFQs [FastQFilePath _ f] ofname = moveOrCopyCompress' (opts {woOFile = ofname}) f
        moveOrCopyCompressFQs multiple ofname = do
            let inputs = fqpathFilePath <$> multiple
            fp' <- makeNGLTempFile (head inputs) "concat" "tmp" $ \h ->
                C.runConduit
                    (mapM_ conduitPossiblyCompressedFile inputs .| C.sinkHandle h)
            moveOrCopyCompress' (opts {woCanMove = True, woOFile = ofname}) fp'
    if woFormatFlags opts == Just "interleaved"
        then
            withOutputFileO opts $ \out ->
                C.runConduitRes (interleaveFQs rs .| out)
        else case rs of
            ReadSet [] singles ->
                liftIO =<< moveOrCopyCompressFQs singles ofile
            ReadSet pairs [] -> do
                fname1 <- _formatFQOname ofile "pair.1"
                fname2 <- _formatFQOname ofile "pair.2"
                cp1 <- moveOrCopyCompressFQs (fst <$> pairs) fname1
                cp2 <- moveOrCopyCompressFQs (snd <$> pairs) fname2
                liftIO $ cp1 `concurrently_` cp2
            ReadSet pairs singletons -> do
                fname1 <- _formatFQOname ofile "pair.1"
                fname2 <- _formatFQOname ofile "pair.2"
                fname3 <- _formatFQOname ofile "singles"
                cp1 <- moveOrCopyCompressFQs (fst <$> pairs) fname1
                cp2 <- moveOrCopyCompressFQs (snd <$> pairs) fname2
                cp3 <- moveOrCopyCompressFQs singletons fname3
                liftIO $ cp1 `concurrently_` cp2 `concurrently_` cp3
    return (NGOFilename ofile)

executeWrite el@(NGOMappedReadSet _ iout  _) args = do
    opts <- parseWriteOptions args
    fp <- asFile iout
    let guessFormat :: String -> NGLessIO T.Text
        guessFormat "/dev/stdout" = return "sam"
        guessFormat ofile
            | endswith ".sam" ofile = return "sam"
            | endswith ".sam.gz" ofile = return "sam"
            | endswith ".sam.bz2" ofile = return "sam"
            | endswith ".sam.zst" ofile = return "sam"
            | endswith ".sam.zstd" ofile = return "sam"
            | endswith ".bam" ofile = return "bam"
            | otherwise = do
                outputListLno' WarningOutput ["Cannot determine format of MappedReadSet output based on filename ('", ofile, "'). Defaulting to BAM."]
                return "bam"
    orig <- maybe (guessFormat (woOFile opts)) return (woFormat opts) >>= \case
        "sam"
            | endswith ".bam" fp -> convertBamToSam fp
            | otherwise -> return fp
        "bam"
            | endswith ".bam" fp -> return fp -- We already have a BAM, so just copy it
            | otherwise -> convertSamToBam fp
        s -> throwScriptError ("write does not accept format {" ++ T.unpack s ++ "} with input type " ++ show el)
    moveOrCopyCompress opts orig
    return (NGOFilename $ woOFile opts)

executeWrite (NGOCounts iout) args = do
    opts <- parseWriteOptions args
    outputListLno' InfoOutput ["Writing counts to: ", woOFile opts]
    comment <- buildComment (woComment opts) (woAutoComment opts) (woHash opts)
    case fromMaybe "tsv" (woFormat opts) of
        "tsv" -> do
            case comment of
                [] -> do
                    fp <- asFile iout
                    moveOrCopyCompress opts fp
                _ -> do
                    let istream = case iout of
                            File fp -> C.sourceFile fp
                            Stream _ _ iss ->
                                iss .| CL.map unlinesVC
                    withOutputFileO opts $ \out ->
                        C.runConduit $
                            (commentC "# " comment >> istream)
                                .| out
        "csv" -> do
            let (_, istream) = asStream iout
            withOutputFileO opts $ \out ->
                C.runConduit $
                    ((commentC "# " comment .| linesVC 1024)
                        >> (istream .| CL.map (V.map tabToComma)))
                    .| CL.map unlinesVC
                    .| out
        f -> throwScriptError ("Invalid format in write: {"++T.unpack f++"}.\n\tWhen writing counts, only accepted values are {tsv} (TAB separated values; default) or {csv} (COMMA separated values).")
    return (NGOFilename $ woOFile opts)
  where
    tabToComma :: ByteLine -> ByteLine
    tabToComma (ByteLine line) = ByteLine $ B8.map (\case { '\t' -> ','; c -> c }) line
    nl = B.singleton 10
    unlinesVC :: V.Vector ByteLine -> B.ByteString
    unlinesVC vs =
        B.concat
            $ concatMap (\(ByteLine ell) -> [ell, nl])
            $ V.toList vs

executeWrite (NGOFilename fp) args = do
    opts <- parseWriteOptions args
    moveOrCopyCompress opts fp
    return (NGOFilename $ woOFile opts)

executeWrite (NGOSequenceSet fp) args = do
    opts <- parseWriteOptions args
    moveOrCopyCompress opts fp
    return $ NGOSequenceSet (woOFile opts)

executeWrite v _ = throwShouldNotOccur ("Error: executeWrite of " ++ show v ++ " not implemented yet.")


