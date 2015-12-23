{-# LANGUAGE TemplateHaskell #-}
module FileManagement
    ( createTempDir
    , generateDirId
    , openNGLTempFile
    , openNGLTempFile'
    , setupHtmlViewer
    , takeBaseNameNoExtensions
    , nglMaybeCopyFile
    ) where

import qualified Data.ByteString as BS
import System.FilePath.Posix
import Control.Monad
import System.Posix.Internals (c_getpid)

import Data.FileEmbed (embedDir)
import Output
import System.Directory
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import Configuration (temporaryFileDirectory, outputDirectory, nConfKeepTemporaryFiles, nglConfiguration)
import NGLess

-- | open a temporary file
-- This respects the preferences of the user (using the correct temporary
-- directory and deleting the file when necessary)
openNGLTempFile' :: FilePath -> String -> String -> NGLessIO (ReleaseKey, (FilePath, Handle))
openNGLTempFile' base prefix ext = do
    tdir <- temporaryFileDirectory
    liftIO $ createDirectoryIfMissing True tdir
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    let cleanupAction = if not keepTempFiles
                then deleteTempFile
                else hClose . snd
    (key,(fp,h)) <- allocate
                (openTempFile tdir (prefix ++ takeBaseNameNoExtensions base ++ "." ++ ext))
                cleanupAction
    outputListLno' DebugOutput ["Created & opened temporary file ", fp]
    return (key,(fp,h))

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

takeBaseNameNoExtensions = dropExtensions . takeBaseName

createTempDir :: FilePath -> NGLessIO (ReleaseKey,FilePath)
createTempDir dst = do
    t <- liftIO getTemporaryDirectory
    allocate
        (do
            fp <- createTempDirectory t (takeBaseNameNoExtensions dst)
            createDirectory fp
            return fp)
        removeDirectoryRecursive

generateDirId :: FilePath -> NGLessIO FilePath
generateDirId dst = do
    odir <- outputDirectory
    liftIO $ createTempDirectory odir (takeBaseNameNoExtensions dst)

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir t = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> t <.> show x
      r <- doesDirectoryExist (dirpath ++ "$temp")
      if r
          then findTempName (x + 1)
          else return dirpath

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


nglMaybeCopyFile :: FilePath -> FilePath -> NGLessIO ()
nglMaybeCopyFile old new
    | new == old = return()
    | otherwise = liftIO (copyFile old new)
