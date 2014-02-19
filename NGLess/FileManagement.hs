module FileManagement
    ( 
        removeFileIfExists,
        createDir,
        getTempFilePath,
        setupRequiredFiles,
        copyFile,
        getFilesInDir,
        getTFilePathComp,
        getTemporaryDirectory,
        getFileSize
    ) where

import System.Posix (getFileStatus, fileSize)
import System.FilePath.Posix
import System.Directory
import System.IO

import Control.Monad

import System.Posix.Internals (c_getpid)


isDot :: FilePath -> Bool
isDot f = f `notElem` [".", ".."]

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir p = do
 files <- getDirectoryContents p
 return $ map ((</>) p) (filter (isDot) files)

setupRequiredFiles :: FilePath -> IO FilePath
setupRequiredFiles destDir = do
    destDir' <- createDir destDir 
    copyFile "Html/index.html" (destDir' ++ "/index.html")
    copyFile "Html/perBaseQualScores.css" (destDir' ++ "/perBaseQualScores.css")
    copyFile "Html/perBaseQualityScores.js" (destDir' ++ "/perBaseQualityScores.js")
    return destDir'


generateTempFilePath :: FilePath -> String -> IO FilePath
generateTempFilePath fd template = do
    res <- openTempFile fd template   
    hClose (snd res)   
    return (fst res)

getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let oldFilePath = splitFileName . fst . splitExtensions $ fp
    generateTempFilePath (fst oldFilePath) (snd oldFilePath)
    

getTFilePathComp :: FilePath -> IO FilePath
getTFilePathComp fp = do
    let oldFilePath = splitFileName . fst . splitExtensions $ fp
    generateTempFilePath (fst oldFilePath) ((snd oldFilePath) ++ ".gz")
    

removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp

-- Removes the destiny directory if it already exists from previous executions.

createDir destDir = do
    tdir <- getTemporaryDirectory
    let template = snd . splitFileName . fst . splitExtensions $ destDir
    createTempDirectory tdir template


createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  fp <- findTempName pid
  createDirectory fp
  return fp
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      r <- doesDirectoryExist dirpath
      case r of
        False  -> return dirpath
        True -> findTempName (x+1)

getFileSize path = getFileStatus path >>= \s -> return $ fileSize s
