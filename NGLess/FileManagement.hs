{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}
module FileManagement
    ( InstallMode(..)
    , createTempDir
    , openNGLTempFile
    , openNGLTempFile'
    , makeNGLTempFile
    , removeIfTemporary
    , setupHtmlViewer
    , takeBaseNameNoExtensions
    , samtoolsBin
    , prodigalBin
    , megahitBin
    , bwaBin
    , minimap2Bin
    , expandPath

    , inferCompression
    , ensureCompressionIsOneOf
    , Compression(..)

#ifdef IS_BUILDING_TEST
    , expandPath'
#endif
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Text.RE.TDFA.String as RE
import qualified System.FilePath as FP
import qualified Data.Conduit.Algorithms.Async as CAlg
import qualified Conduit as C
import           Conduit ((.|))
import           System.FilePath (takeBaseName, takeDirectory, (</>), (<.>), (-<.>))
import           Control.Monad (unless, forM_, when)
import           System.Posix.Files (setFileMode)
import           System.Posix.Internals (c_getpid)
import           Data.List (isSuffixOf, isPrefixOf)

import System.Directory
import System.IO
import System.IO.Error
import Control.Exception
import System.Environment (getExecutablePath, lookupEnv)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

import Configuration
import Version (versionStr)

import Data.FileEmbed (embedDir)
import Output
import NGLess.NGLEnvironment
import NGLess.NGError
import Dependencies.Embedded
import Utils.LockFile
import Utils.Utils (withOutputFile)


{- Note on temporary files
 -
 - A big part of this module is handling temporary files. By generating
 - temporary files with the functions in this module, one guarantees that user
 - settings (wrt where to store files, whether to keep them, &c) are respected.
 - It also enables garbage collection.
 -
 -}

data InstallMode = User | Root deriving (Eq, Show)

data Compression = NoCompression
                | GzipCompression
                | BZ2Compression
                | XZCompression
                | ZStdCompression
                deriving (Eq)

inferCompression :: FilePath -> Compression
inferCompression fp
    | isSuffixOf ".gz" fp = GzipCompression
    | isSuffixOf ".bz2" fp = BZ2Compression
    | isSuffixOf ".xz"  fp = XZCompression
    | isSuffixOf ".zst" fp = ZStdCompression
    | isSuffixOf ".zstd" fp = ZStdCompression
    | otherwise = NoCompression


{- Ensure that the file is compressed using an acceptable compression format
 - (potentially by recompressing).
 -}
ensureCompressionIsOneOf :: [Compression] -- ^ Acceptable formats
                                -> FilePath -- ^ input file
                                -> NGLessIO FilePath
ensureCompressionIsOneOf [] fp = return fp
ensureCompressionIsOneOf cs fp
        | inferCompression fp `elem` cs = return fp
        | otherwise = makeNGLTempFile fp "adjust_compression_" ext' $ \h ->
            CAlg.withPossiblyCompressedFile fp $ \src ->
                C.runConduit $
                    src .| (if GzipCompression `elem` cs
                        then CAlg.asyncGzipTo
                        else C.sinkHandle) h
    where
        ext' = case FP.takeExtensions fp of
                "" -> ""
                (_:rest)
                    | GzipCompression `elem` cs -> rest -<.> "gz"
                    | otherwise -> rest


-- | Shorten filename if longer than 240 characters
-- If base + ext is above 240 chars, avoid reaching the system limit of 255
-- by shortening the middle part of the filename
checkFilenameLength :: FilePath -> String -> String
checkFilenameLength base ext = if len > 240
                                  then shorten base
                                  else base
    where len = length base + length ext
          -- Take first 1/3 and last 1/3 of the original base name
          shorten f = take (div len 3) f ++ "..." ++ drop (div (2 * len) 3) f

-- | open a temporary file
-- This respects the preferences of the user (using the correct temporary
-- directory and deleting the file when necessary)
--
-- These files will be auto-removed when ngless exits
openNGLTempFile' :: FilePath -- ^ basename
                        -> String -- ^ prefix
                        -> String -- ^ extension
                        -> NGLessIO (ReleaseKey, (FilePath, Handle))
openNGLTempFile' base prefix ext = do
    tdir <- nConfTemporaryDirectory <$> nglConfiguration
    liftIO $ createDirectoryIfMissing True tdir
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    let cleanupAction = if not keepTempFiles
                then deleteTempFile
                else hClose . snd

        filename = checkFilenameLength base ext
    (key,(fp,h)) <- allocate
                (openTempFileWithDefaultPermissions tdir (prefix ++ takeBaseNameNoExtensions filename ++ "." ++ ext))
                cleanupAction
    outputListLno' DebugOutput ["Created & opened temporary file ", fp]
    updateNglEnvironment $ \e -> e { ngleTemporaryFilesCreated = fp : ngleTemporaryFilesCreated e }
    return (key,(fp,h))

-- | Open a temporary file
-- See openNGLTempFile'
openNGLTempFile :: FilePath -> String -> String -> NGLessIO (FilePath, Handle)
openNGLTempFile base pre ext = snd <$> openNGLTempFile' base pre ext

deleteTempFile (fp, h) = do
    hClose h
    removeFileIfExists fp

-- | Create a temporary file
-- The Handle is closed after the action is performed
--
-- See 'openNGLTempFile'
makeNGLTempFile :: FilePath -> String -> String -> (Handle -> NGLessIO ()) -> NGLessIO FilePath
makeNGLTempFile base pre ext act = do
    (fpath, h) <- openNGLTempFile base pre ext
    act h
    liftIO $ hClose h
    return fpath

-- | removeIfTemporary removes files if (and only if):
-- 1. this was a temporary file created by 'openNGLTempFile'
-- 2. the user has not requested that temporary files be kept
--
-- It is *not necessary* to call this function, but it may save on temporary
-- disk space to clean up early.
removeIfTemporary :: FilePath -> NGLessIO ()
removeIfTemporary fp = do
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    createdFiles <- ngleTemporaryFilesCreated <$> nglEnvironment
    unless (keepTempFiles || fp `notElem` createdFiles) $ do
        outputListLno' DebugOutput ["Removing temporary file: ", fp]
        liftIO $ removeFileIfExists fp
        updateNglEnvironment $ \e -> e { ngleTemporaryFilesCreated = filter (/=fp) (ngleTemporaryFilesCreated e) }
{-# NOINLINE removeIfTemporary #-}


-- | This is a version of 'takeBaseName' which drops all extension
-- ('takeBaseName' only takes out the first extension)
takeBaseNameNoExtensions = FP.dropExtensions . FP.takeBaseName
{-# INLINE takeBaseNameNoExtensions #-}

-- | create a temporary directory as a sub-directory of the user-specified
-- temporary directory.
--
-- Releasing deletes the directory and all its contents (unless the user
-- requested that temporary files be preserved).
createTempDir :: String -> NGLessIO (ReleaseKey,FilePath)
createTempDir template = do
        tbase <- nConfTemporaryDirectory <$> nglConfiguration
        liftIO $ createDirectoryIfMissing True tbase
        keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
        allocate
            (c_getpid >>= createFirst tbase (takeBaseNameNoExtensions template))
            (if not keepTempFiles
                then removeDirectoryRecursive
                else const (return ()))
    where
        createFirst :: (Num a, Show a) => FilePath -> String -> a -> IO FilePath
        createFirst dirbase t n = do
            let dirpath = dirbase </> t <.> "tmp" ++ show n
            try (createDirectory dirpath) >>= \case
                Right () -> return dirpath
                Left e
                    | isAlreadyExistsError e -> createFirst dirbase t (n + 1)
                    | otherwise -> ioError e

-- This is in IO because it is run after NGLessIO has finished.
setupHtmlViewer :: FilePath -> IO ()
setupHtmlViewer dst = do
    exists <- doesFileExist (dst </> "index.html")
    unless exists $ do
        createDirectoryIfMissing False dst
        forM_ $(embedDir "Html") $ \(fp,bs) ->
            B.writeFile (dst </> fp) bs


-- | path to bwa
bwaBin :: NGLessIO FilePath
bwaBin = findOrCreateBin "NGLESS_BWA_BIN" bwaFname bwaData
    where
        bwaFname = "ngless-" ++ versionStr ++ "-bwa" ++ binaryExtension

-- | path to samtools
samtoolsBin :: NGLessIO FilePath
samtoolsBin = findOrCreateBin "NGLESS_SAMTOOLS_BIN" samtoolsFname samtoolsData
    where
        samtoolsFname = "ngless-" ++ versionStr ++ "-samtools" ++ binaryExtension
        --
-- | path to prodigal
prodigalBin :: NGLessIO FilePath
prodigalBin = findOrCreateBin "NGLESS_PRODIGAL_BIN" prodigalFname prodigalData
    where
        prodigalFname = "ngless-" ++ versionStr ++ "-prodigal" ++ binaryExtension

-- | path to minimap2
minimap2Bin :: NGLessIO FilePath
minimap2Bin = findOrCreateBin "NGLESS_MINIMAP2_BIN" minimap2Fname minimap2Data
    where
        minimap2Fname = "ngless-" ++ versionStr ++ "-minimap2" ++ binaryExtension


megahitBin :: NGLessIO FilePath
megahitBin = liftIO (lookupEnv "NGLESS_MEGAHIT_BIN") >>= \case
    Just bin -> checkExecutable "NGLESS_MEGAHIT_BIN" bin
    Nothing -> do
        path <- findBin ("ngless-"++versionStr ++ "-megahit/megahit")
        maybe createMegahitBin return path

createMegahitBin :: NGLessIO FilePath
createMegahitBin = do
    megahitData' <- liftIO megahitData
    destdir <- (</> ("ngless-" ++ versionStr ++ "-megahit")) <$> binPath User
    when (B.null megahitData') $
        throwSystemError "Cannot find megahit on the system and this is a build without embedded dependencies."
    liftIO $ createDirectoryIfMissing True destdir
    withLockFile LockParameters
                { lockFname = destdir ++ "lock.megahit-expand"
                , maxAge = 300
                , whenExistsStrategy = IfLockedRetry { nrLockRetries = 37*60, timeBetweenRetries = 60 }
                , mtimeUpdate = True
               } $ do
        outputListLno' TraceOutput ["Expanding megahit binaries into ", destdir]
        unpackMegahit destdir $ Tar.read . GZip.decompress $ BL.fromChunks [megahitData']
    return $ destdir </> "megahit"
    where
        unpackMegahit :: FilePath -> Tar.Entries Tar.FormatError -> NGLessIO ()
        unpackMegahit _ Tar.Done = return ()
        unpackMegahit _ (Tar.Fail err) = throwSystemError ("Error expanding megahit archive: " ++ show err)
        unpackMegahit destdir (Tar.Next e next) = do
            case Tar.entryContent e of
                Tar.NormalFile content _ -> do
                    let dest = destdir </> takeBaseName (Tar.entryPath e)
                    liftIO $ do
                        BL.writeFile dest content
                        --setModificationTime dest (posixSecondsToUTCTime (fromIntegral $ Tar.entryTime e))
                        setFileMode dest (Tar.entryPermissions e)
                Tar.Directory -> return ()
                _ -> throwSystemError ("Unexpected entry in megahit tarball: " ++ show e)
            unpackMegahit destdir next


binPath :: InstallMode -> NGLessIO FilePath
binPath Root = do
    nglessBinDirectory <- takeDirectory <$> liftIO getExecutablePath
#ifndef WINDOWS
    return (nglessBinDirectory </> "../share/ngless/bin")
#else
    return nglessBinDirectory
#endif
binPath User = ((</> "bin") . nConfUserDirectory) <$> nglConfiguration

findBin :: FilePath -> NGLessIO (Maybe FilePath)
findBin fname = do
        rootPath <- (</> fname) <$> binPath Root
        rootex <- liftIO $ canExecute rootPath
        if rootex then
            return (Just rootPath)
        else do
            userpath <- (</> fname) <$> binPath User
            userex <- liftIO $ canExecute userpath
            return $ if userex
                then Just userpath
                else Nothing
    where
        canExecute bin = do
            exists <- doesFileExist bin
            if exists
                then executable <$> getPermissions bin
                else return False

writeBin :: FilePath -> IO B.ByteString -> NGLessIO FilePath
writeBin fname bindata = do
    userBinPath <- binPath User
    bindata' <- liftIO bindata
    when (B.null bindata') $
        throwSystemError ("Cannot find " ++ fname ++ " on the system and this is a build without embedded dependencies.")
    liftIO $ createDirectoryIfMissing True userBinPath
    let fname' = userBinPath </> fname
    withLockFile LockParameters
                    { lockFname = fname' ++ ".expand.lock"
                    , maxAge = 300
                    , whenExistsStrategy = IfLockedRetry { nrLockRetries = 60, timeBetweenRetries = 60 }
                    , mtimeUpdate = True
                    } $ liftIO $ do
        withOutputFile fname' (flip B.hPut bindata')
        p <- getPermissions fname'
        setPermissions fname' (setOwnerExecutable True p)
        return fname'

findOrCreateBin :: String -> FilePath -> IO B.ByteString -> NGLessIO FilePath
findOrCreateBin envvar fname bindata = liftIO (lookupEnv envvar) >>= \case
    Just bin -> checkExecutable envvar bin
    Nothing -> do
        path <- findBin fname
        maybe (writeBin fname bindata) return path

checkExecutable :: String -> FilePath -> NGLessIO FilePath
checkExecutable name bin = do
    exists <- liftIO $ doesFileExist bin
    unless exists
        (throwSystemError $ concat [name, " binary not found!\n","Expected it at ", bin])
    is_executable <- executable <$> liftIO (getPermissions bin)
    unless is_executable
        (throwSystemError $ concat [name, " binary found at ", bin, ".\nHowever, it is not an executable file!"])
    return bin


expandPath :: FilePath -> NGLessIO (Maybe FilePath)
expandPath fbase = do
        searchpath <- nConfSearchPath <$> nglConfiguration
        outputListLno' TraceOutput ["Looking for file '", fbase, "' (search path is ", show searchpath, ")"]
        let candidates = expandPath' fbase searchpath
        findMaybeM candidates $ \p -> do
            outputListLno' TraceOutput ["Looking for file (", fbase, ") in ", p]
            exists <- liftIO (doesFileExist p)
            return $! if exists
                            then Just p
                            else Nothing
    where
        findMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
        findMaybeM [] _ = return Nothing
        findMaybeM (x:xs) f = f x >>= \case
            Nothing -> findMaybeM xs f
            val -> return val

expandPath' :: FilePath -> [FilePath] -> [FilePath]
expandPath' fbase search = case RE.matchedText $ fbase RE.?=~ [RE.re|<(@{%id})?>|] of
        Nothing -> [fbase]
        Just c -> mapMaybe (expandPath'' $ trim c) search
    where
        trim = init . drop 1
        expandPath'' :: FilePath -> FilePath -> Maybe FilePath
        expandPath'' code path = (</> fbase') <$> simplify code path
        simplify :: FilePath -> FilePath -> Maybe FilePath
        simplify c path
            | '=' `notElem` path = Just path
            | (c++"=")`isPrefixOf` path = Just $ drop (length c + 1) path
            | otherwise = Nothing
        fbase' = removeSlash1 $ fbase RE.*=~/ [RE.ed|<(@{%id})?>///|]
        removeSlash1 "" = ""
        removeSlash1 ('/':p) = removeSlash1 p
        removeSlash1 p = p

binaryExtension :: String
#ifdef WINDOWS
binaryExtension = ".exe"
#else
binaryExtension = ""
#endif
