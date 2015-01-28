{-# LANGUAGE TemplateHaskell #-}
module FileManagement
    ( createTempDir
    , generateDirId
    , openNGLTempFile
    , setupHtmlViewer
    , readPossiblyCompressedFile
    , takeBaseNameNoExtensions
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS

import qualified Codec.Compression.GZip as GZip    

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)

import System.FilePath.Posix
import System.Directory
import System.IO

import Control.Monad
import System.Posix.Internals (c_getpid)

import Data.FileEmbed
import Configuration (outputDirectory, temporaryFileDirectory)

-- 
openNGLTempFile :: FilePath -> String -> String -> IO (FilePath, Handle)
openNGLTempFile base prefix ext = do
    tdir <- temporaryFileDirectory
    openTempFile tdir (prefix ++ takeBaseNameNoExtensions base ++ "." ++ ext)

takeBaseNameNoExtensions = dropExtensions . takeBaseName
    
createTempDir :: FilePath -> IO FilePath
createTempDir dst = do
    t <- getTemporaryDirectory
    fp <- createTempDirectory t (takeBaseNameNoExtensions dst)
    createDirectory fp
    return fp

generateDirId :: FilePath -> IO FilePath
generateDirId dst = do
    odir <- outputDirectory
    createTempDirectory odir (takeBaseNameNoExtensions dst)
    
createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir t = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> t <.> show x
      r <- doesDirectoryExist (dirpath ++ "$beforeQC")
      case r of
        False  -> return dirpath
        True -> findTempName (x+1)

setupHtmlViewer :: FilePath -> IO ()
setupHtmlViewer dst = do
    exists <- doesFileExist (dst </> "nglessKeeper.html")
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


readPossiblyCompressedFile ::  FilePath -> IO BL.ByteString
readPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = GZip.decompress <$> BL.readFile fname
    | otherwise = BL.readFile fname

