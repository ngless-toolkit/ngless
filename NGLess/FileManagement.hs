{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell #-}
module FileManagement
    ( createTempDir
    , getFileSize
    , openNGLTempFile
    , openNGLTempFile'
    , removeFileIfExists
    , removeIfTemporary
    , setupHtmlViewer
    , takeBaseNameNoExtensions
    ) where

import qualified Data.ByteString as BS
import System.FilePath
import Control.Monad
import System.Posix.Internals (c_getpid)
import System.Posix (getFileStatus, fileSize, FileOffset)

import Data.FileEmbed (embedDir)
import Output
import System.Directory
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import Configuration (nConfTemporaryDirectory, nConfKeepTemporaryFiles, nglConfiguration)

import NGLess.NGLEnvironment
import NGLess.NGError

-- | open a temporary file
-- This respects the preferences of the user (using the correct temporary
-- directory and deleting the file when necessary)
openNGLTempFile' :: FilePath -> String -> String -> NGLessIO (ReleaseKey, (FilePath, Handle))
openNGLTempFile' base prefix ext = do
    tdir <- nConfTemporaryDirectory <$> nglConfiguration
    liftIO $ createDirectoryIfMissing True tdir
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    let cleanupAction = if not keepTempFiles
                then deleteTempFile
                else hClose . snd
    (key,(fp,h)) <- allocate
                (openTempFileWithDefaultPermissions tdir (prefix ++ takeBaseNameNoExtensions base ++ "." ++ ext))
                cleanupAction
    outputListLno' DebugOutput ["Created & opened temporary file ", fp]
    updateNglEnvironment $ \e -> e { ngleTemporaryFilesCreated = fp:(ngleTemporaryFilesCreated e) }
    return (key,(fp,h))

-- | See openNGLTempFile'
openNGLTempFile :: FilePath -> String -> String -> NGLessIO (FilePath, Handle)
openNGLTempFile base pre ext = snd <$> openNGLTempFile' base pre ext

removeFileIfExists fp = removeFile fp `catch` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e
deleteTempFile (fp, h) = do
    hClose h
    removeFileIfExists fp

-- removeIfTemporary takes a file path and if this file was created as an
-- ngless temporary file and unless the user requested that temporary files be
-- kept, then the file is removed.
removeIfTemporary :: FilePath -> NGLessIO ()
removeIfTemporary fp = do
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    createdFiles <- ngleTemporaryFilesCreated <$> nglEnvironment
    unless (keepTempFiles || fp `notElem` createdFiles) $ do
        outputListLno' DebugOutput ["Removing temporary file: ", fp]
        liftIO $ removeFileIfExists fp
        updateNglEnvironment $ \e -> e { ngleTemporaryFilesCreated = filter (==fp) (ngleTemporaryFilesCreated e) }


-- takeBaseName only takes out the first extension
takeBaseNameNoExtensions = dropExtensions . takeBaseName


-- | create a temporary directory as a sub-directory of the user-specified
-- temporary directory. Releasing deletes the directory and all its contents.
createTempDir :: String -> NGLessIO (ReleaseKey,FilePath)
createTempDir template = do
        tbase <- nConfTemporaryDirectory <$> nglConfiguration
        liftIO $ createDirectoryIfMissing True tbase
        allocate
            (c_getpid >>= createFirst tbase (takeBaseNameNoExtensions template))
            removeDirectoryRecursive
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
        createDirectoryIfMissing False (dst </> "htmllibs")
        createDirectoryIfMissing False (dst </> "fonts")
        createDirectoryIfMissing False (dst </> "forms")
        forM_ $(embedDir "Html") $ \(fp,bs) ->
            BS.writeFile (dst </> fp) bs

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing False dst
  xs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents src
  forM_ xs $ \n -> do
    exists <- doesDirectoryExist (src </> n)
    if exists
        then copyDir  (src </> n) (dst </> n)
        else copyFile (src </> n) (dst </> n)

getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path
