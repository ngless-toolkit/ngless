{-# LANGUAGE TemplateHaskell #-}
module FileManagement
    ( 
        createDir,
        getTempFilePath,
        getFilesInDir,
        getTFilePathComp,
        getTemporaryDirectory,
        generateDirId,
        setupHtmlViewer,
        readPossiblyCompressedFile,
        writeGZIP,
        parseFileName,
        template
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
import Configuration (outputDirectory)

isNotDot :: FilePath -> Bool
isNotDot f = f `notElem` [".", ".."]

---- Files in a Directory
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir p = do
 files <- getDirectoryContents p
 return $ map ((</>) p) (filter isNotDot files)


-- 
generateTempFilePath :: FilePath -> String -> IO FilePath
generateTempFilePath dst t = do
    (f,s) <- openTempFile dst t   
    hClose s   
    return f

--Example: "folders/sample_1.9168$afterQC" @?= ("folders/","sample_1")
parseFileName :: FilePath -> (FilePath, FilePath)
parseFileName = splitFileName . fst . break ((==) '$') . fst . splitExtensions

getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let (dst, t) = parseFileName fp
    generateTempFilePath dst t
    
getTFilePathComp :: FilePath -> IO FilePath
getTFilePathComp fp = do
    let (dst, t) = parseFileName fp
    generateTempFilePath dst (t <.> "gz")

---- generate template from path
template :: FilePath -> FilePath
template = snd . splitFileName . fst . splitExtensions
    
createDir :: FilePath -> IO FilePath
createDir dst = do
    fp <- getTemporaryDirectory >>= flip createTempDirectory (template dst)
    createDirectory fp
    return fp

generateDirId :: FilePath -> FilePath -> IO FilePath
generateDirId fname dst = do
    odir <- outputDirectory fname
    createTempDirectory odir (template dst)
    
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
setupHtmlViewer fname = do
    dst <- outputDirectory fname
    let p = (dst </> "nglessKeeper.html")
    exists <- doesFileExist p
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
  xs <- filter isNotDot <$> getDirectoryContents src
  forM_ xs $ \n -> do
    exists <- doesDirectoryExist (src </> n)
    if exists
        then copyDir  (src </> n) (dst </> n)
        else copyFile (src </> n) (dst </> n)



writeGZIP :: String -> BL.ByteString -> IO ()
writeGZIP fp contents = BL.writeFile fp $ GZip.compress contents 

readPossiblyCompressedFile ::  FilePath -> IO BL.ByteString
readPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = GZip.decompress <$> BL.readFile fname
    | otherwise = BL.readFile fname

