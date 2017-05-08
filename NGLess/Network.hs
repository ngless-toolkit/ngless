{- Copyright 2013-2017 NGLess Authors
 - License: MIT
 -}
module Network
    ( downloadFile
    , downloadOrCopyFile
    , downloadExpandTar
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import           Data.Conduit (($$+-), (=$=))

import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP

import Data.Conduit.Binary (sinkFile)
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import Data.List (isPrefixOf)
import System.Posix.Files (setFileMode)
import System.FilePath

import Output
import NGLess
import Utils.Conduit
import Utils.ProgressBar

downloadOrCopyFile :: FilePath -> FilePath -> NGLessIO ()
downloadOrCopyFile src dest
    | any (`isPrefixOf` src) ["http://", "https://", "ftp://"] = downloadFile src dest
    | otherwise = liftIO $ copyFile src dest


downloadFile :: String -> FilePath -> NGLessIO ()
downloadFile url destPath = do
    outputListLno' TraceOutput ["Downloading ", url]
    req <- HTTP.parseRequest url
    manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    let req' = req { HTTP.decompress = const False }
    res <- HTTP.http req' manager

    case lookup "Content-Length" (HTTP.responseHeaders res) of
        Nothing -> throwSystemError "HTTP Response does not contain Content-Length header"
        Just csize ->
            HTTP.responseBody res
                $$+- printProgress (read (B.unpack csize))
                =$= sinkFile destPath

-- Download a tar.gz file and expand it onto 'destdir'
downloadExpandTar :: FilePath -> FilePath -> NGLessIO ()
downloadExpandTar url destdir = do
    let tarName = destdir <.> "tar.gz"

    liftIO $ createDirectoryIfMissing True destdir
    downloadOrCopyFile url tarName
    expandTar . Tar.read . GZip.decompress =<< liftIO (BL.readFile tarName)
    liftIO $ removeFile tarName
  where
    -- We cannot use Tar.unpack as that function does not correctly set permissions
    expandTar :: Tar.Entries Tar.FormatError -> NGLessIO ()
    expandTar Tar.Done = return ()
    expandTar (Tar.Fail err) = throwSystemError ("Error expanding archive: " ++ show err)
    expandTar (Tar.Next e next) = do
          case Tar.entryContent e of
              Tar.NormalFile content _ -> do
                  let dest = destdir </> Tar.entryPath e
                  outputListLno' TraceOutput ["Expanding ", dest]
                  liftIO $ do
                      createDirectoryIfMissing True (takeDirectory dest)
                      BL.writeFile dest content
                      --setModificationTime dest (posixSecondsToUTCTime (fromIntegral $ Tar.entryTime e))
                      setFileMode dest (Tar.entryPermissions e)
              Tar.Directory -> return ()
              _ -> throwSystemError ("Unexpected entry in megahit tarball: " ++ show e)
          expandTar next

printProgress :: Int -> C.Conduit B.ByteString NGLessIO B.ByteString
printProgress csize = liftIO (mkProgressBar 40) >>= loop 0
  where
    loop !len pbar = awaitJust $ \bs -> do
            let len' = len + B.length bs
                progress = fromIntegral len' / fromIntegral csize
            pbar' <- liftIO (updateProgressBar pbar progress)
            C.yield bs
            loop len' pbar'


