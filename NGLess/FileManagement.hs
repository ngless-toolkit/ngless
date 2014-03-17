module FileManagement
    ( 
        removeFileIfExists,
        createDir,
        getTempFilePath,
        copyFile,
        getFilesInDir,
        getTFilePathComp,
        getTemporaryDirectory,
        getFileSize,
        setupRequiredFiles,
        openKFileHandles,
        closekFileHandles,
        numFiles,
        doesDirContainFormats,
        printNglessLn,
        createOutputDir,
        setupHtmlViewer,
        defaultDir,
        doesFileExist,
        createDirIfExists
    ) where

import System.FilePath.Posix
import System.Directory
import System.IO

import Control.Monad

import System.Posix.Internals (c_getpid)

import System.Console.CmdArgs.Verbosity


maxFileSize :: Num a => a
maxFileSize = 300000000 -- 100MB

calcSize :: Integer -> Integer
calcSize s = ceiling $ ((fromInteger s) / maxFileSize :: Double) 

numFiles :: FilePath -> IO Integer
numFiles path = do
    size' <- getFileSize path
    return $ calcSize size'

openKFileHandles :: Int -> FilePath -> IO [Handle]
openKFileHandles k dest = do
    forM [0..k - 1] $ \x -> do
        openFile (dest </> (show x)) AppendMode

closekFileHandles :: [Handle] -> IO ()
closekFileHandles fhs = mapM_ (hClose) fhs

defaultDir :: IO String
defaultDir = do 
  tdir <- getTemporaryDirectory
  return $ tdir </> "ngless.outputs/"

isDot :: FilePath -> Bool
isDot f = f `notElem` [".", ".."]

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir p = do
 files <- getDirectoryContents p
 return $ map ((</>) p) (filter (isDot) files)


setupRequiredFiles :: FilePath -> FilePath -> IO FilePath
setupRequiredFiles info dirTemplate = do
    let destDir' = dirTemplate ++ "$" ++ info
    createDirectory destDir'
    copyFile "Html/perBaseQualScores.css" (destDir' ++ "/perBaseQualScores.css")
    copyFile "Html/perBaseQualityScores.js" (destDir' ++ "/perBaseQualityScores.js")
    case info of
        "beforeQC" -> copyFile "Html/beforeQC.html" (destDir' ++ "/index.html")
        "afterQC" -> copyFile "Html/afterQC.html" (destDir' ++ "/index.html")
        err -> error ("Has to be either before or after QC. it is: " ++ (show err)) 

-- setup lib directory --
--    createDirectory $ destDir' </> "lib"
--    copyFile "Html/lib/d3.v3.js" (destDir' </> "lib/d3.v3.js")
--    copyFile "Html/lib/nv.d3.js" (destDir' </> "lib/nv.d3.js")
--    copyFile "Html/lib/nv.d3.css" (destDir' </> "lib/nv.d3.css")

    return destDir'

generateTempFilePath :: FilePath -> String -> IO FilePath
generateTempFilePath fd template = do
    res <- openTempFile fd template   
    hClose (snd res)   
    return (fst res)

getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let oldFilePath = splitFileName . fst . break ((==) '$') . fst . splitExtensions $ fp
    generateTempFilePath (fst oldFilePath) (snd oldFilePath)
    

getTFilePathComp :: FilePath -> IO FilePath
getTFilePathComp fp = do
    let oldFilePath = splitFileName . fst . break ((==) '$') . fst . splitExtensions $ fp
    generateTempFilePath (fst oldFilePath) ((snd oldFilePath) ++ ".gz")
    

removeFileIfExists fp = do    
    fexist' <- doesFileExist fp
    when fexist' $ removeFile fp

-- Removes the destiny directory if it already exists from previous executions.

createDir destDir = do
    tdir <- getTemporaryDirectory
    let template = snd . splitFileName . fst . splitExtensions $ destDir
    fp <- createTempDirectory tdir template
    createDirectory fp
    return fp

createOutputDir destDir = do
    let template = snd . splitFileName . fst . splitExtensions $ destDir
    tdir' <- defaultDir
    createTempDirectory tdir' template >>= return
    

createDirIfExists tdir =  do
  exists <- doesDirectoryExist tdir
  when (not exists) $ createDirectory tdir 


createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  fp <- findTempName pid
  return fp
  where
    findTempName x = do
      let dirpath = dir </> template ++ "." ++ show x
      r <- doesDirectoryExist dirpath
      case r of
        False  -> return dirpath
        True -> findTempName (x+1)

getFileSize path = withFile path ReadMode hFileSize

doesDirContainFormats :: String -> [String] -> IO Bool
doesDirContainFormats _ [] = return True
doesDirContainFormats path (x:xs) = do 
    x' <- doesFileExist (path ++ x)
    case x' of 
        True -> doesDirContainFormats path xs
        False -> return False

printNglessLn :: String -> IO ()
printNglessLn x = whenLoud $ putStrLn x 

setupHtmlViewer :: IO ()
setupHtmlViewer = do
    dir <- defaultDir
    doesExist <- doesFileExist (dir </> "nglessKeeper.html")
    case doesExist of 
        True   -> return ()
        False  -> do copyFile "Html/nglessKeeper.html" (dir </> "nglessKeeper.html")
                     copyFile "Html/nglessKeeper.css"  (dir </> "nglessKeeper.css")