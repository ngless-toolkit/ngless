module FileManagement
    ( 
        removeFileIfExists,
        createDir,
        getTempFilePath,
        setupRequiredFiles,
        copyFile,
        getFilesInDir,
        generateTempFilePath,
        getTFilePathComp
    ) where

import System.FilePath.Posix
import System.Directory
import System.IO

import Control.Monad


isDot :: FilePath -> Bool
isDot f = f `notElem` [".", ".."]

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir p = do
 files <- getDirectoryContents p
 return $ map ((++) p) (filter (isDot) files)

setupRequiredFiles :: FilePath -> IO ()
setupRequiredFiles destDir = do
    createDir destDir 
    copyFile "Html/index.html" (destDir ++ "/index.html")
    copyFile "Html/perBaseQualScores.css" (destDir ++ "/perBaseQualScores.css")
    copyFile "Html/perBaseQualityScores.js" (destDir ++ "/perBaseQualityScores.js")


generateTempFilePath :: FilePath -> FilePath -> IO FilePath
generateTempFilePath fd fn = do
    res <- openTempFile fd (snd $ splitFileName fn)   
    hClose (snd res)   
    return (fst res)

getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let oldFilePath = splitFileName fp
    res <- openTempFile (fst oldFilePath) (snd oldFilePath)
    hClose (snd res)   
    return (fst res)


getTFilePathComp :: FilePath -> IO FilePath
getTFilePathComp fp = do
    let oldFilePath = splitFileName fp
    res <- openTempFile (fst oldFilePath) ((snd oldFilePath) ++ ".gz")
    hClose (snd res)   
    return (fst res)


removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp

-- Removes the destiny directory if it already exists from previous executions.

createDir destDir = do
    doesDirExist <- doesDirectoryExist destDir
    when (doesDirExist) $ removeDirectoryRecursive destDir
    createDirectory destDir