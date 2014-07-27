module FileManagement
    ( 
        removeFileIfExists,
        createDir,
        getTempFilePath,
        copyFile,
        getFilesInDir,
        getTFilePathComp,
        getTemporaryDirectory,
        setupRequiredFiles,
        openKFileHandles,
        closekFileHandles,
        doesDirContainFormats,
        printNglessLn,
        createOutputDir,
        setupHtmlViewer,
        doesFileExist,
        createDirIfExists,
        readPossiblyCompressedFile,
        unCompress,
        writeGZIP,
        appendFile',
        write,
        parseFileName
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import qualified Codec.Compression.GZip as GZip    

import qualified Data.Text as T

import System.FilePath.Posix
import System.Directory
import System.IO

import Control.Monad

import System.Posix.Internals (c_getpid)

import System.Console.CmdArgs.Verbosity
import Data.DefaultValues


openKFileHandles :: Int -> FilePath -> IO [Handle]
openKFileHandles k dest = do
    forM [0..k - 1] $ \x -> do
        openFile (dest </> (show x)) AppendMode

closekFileHandles :: [Handle] -> IO ()
closekFileHandles fhs = mapM_ (hClose) fhs


isDot :: FilePath -> Bool
isDot f = f `elem` [".", ".."]

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir p = do
 files <- getDirectoryContents p
 return $ map ((</>) p) (filter (not . isDot) files)


setupRequiredFiles :: FilePath -> FilePath -> IO FilePath
setupRequiredFiles info dirTemplate = do
    let destDir' = dirTemplate ++ "$" ++ info
    htmlSourceP <- htmlDefaultDir
    createDirectory destDir'
    copyFile (htmlSourceP </> "perBaseQualScores.css") (destDir' </> "perBaseQualScores.css")
    copyFile (htmlSourceP </> "perBaseQualityScores.js") (destDir' </> "perBaseQualityScores.js")
    case info of
        "beforeQC" -> copyFile (htmlSourceP </> "beforeQC.html") (destDir' </> "index.html")
        "afterQC" -> copyFile (htmlSourceP </> "afterQC.html") (destDir' </> "index.html")
        err -> error ("Has to be either before or after QC. it is: " ++ (show err))
    return destDir'

generateTempFilePath :: FilePath -> String -> IO FilePath
generateTempFilePath fd template = do
    res <- openTempFile fd template   
    hClose (snd res)   
    return (fst res)

parseFileName :: FilePath -> (FilePath, FilePath)
parseFileName = splitFileName . fst . break ((==) '$') . fst . splitExtensions

getTempFilePath :: FilePath -> IO FilePath
getTempFilePath fp = do
    let oldFilePath = parseFileName fp
    generateTempFilePath (fst oldFilePath) (snd oldFilePath)
    

getTFilePathComp :: FilePath -> IO FilePath
getTFilePathComp fp = do
    let oldFilePath = parseFileName fp
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
      r <- doesDirectoryExist (dirpath ++ "$" ++ "beforeQC")
      case r of
        False  -> return dirpath
        True -> findTempName (x+1)

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
    htmlP <- htmlDefaultDir
    dir <- defaultDir
    doesFileExist (p' dir) >>= \x -> case x of 
        True   -> return ()
        False  -> do copyFile (htmlP </> "nglessKeeper.html") (dir </> "nglessKeeper.html")
                     copyFile (htmlP </> "nglessKeeperafterQC.html") (dir </> "nglessKeeperafterQC.html")
                     copyFile (htmlP </> "nglessKeeperbeforeQC.html") (dir </> "nglessKeeperbeforeQC.html")
                     copyFile (htmlP </> "nglessKeepervisualize.html") (dir </> "nglessKeepervisualize.html")
                     copyFile (htmlP </> "nglessKeeper.css")  (dir </> "nglessKeeper.css")
                     copyDir  (htmlP </> htmlDefaultDirLibs)  (dir </> htmlDefaultDirLibs)
                     copyDir  (htmlP </> htmlDefaultFonts)  (dir </> htmlDefaultFonts)
    where p' = (</> "nglessKeeper.html")

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (not . isDot) content
  forM_ xs $ \name -> do
    copyFile (src </> name) (dst </> name)


readPossiblyCompressedFile ::  B.ByteString -> IO BL.ByteString
readPossiblyCompressedFile fileName = unCompress (B.unpack fileName)


writeGZIP :: String -> BL.ByteString -> IO ()
writeGZIP fp contents = BL.writeFile fp $ GZip.compress contents 

write :: String -> BL.ByteString -> IO ()
write fp contents = BL.writeFile fp contents 

unCompress :: FilePath -> IO BL.ByteString
unCompress fname =
    if T.isInfixOf (T.pack ".gz") (T.pack fname)
        then fmap GZip.decompress (BL.readFile fname)
        else BL.readFile fname -- not compressed


appendFile' = BL.hPutStrLn
