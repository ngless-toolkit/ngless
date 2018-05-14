{- Copyright 2013-2018 NGLess Authors
 - License: MIT
 -}
module Utils.Network
    ( downloadFile
    , downloadOrCopyFile
    , downloadExpandTar
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))

import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTPSimple

import qualified Data.Conduit.Binary as CB
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
    let req' = req { HTTP.decompress = const False }
    r <- liftIO $ HTTPSimple.withResponse req' $ \res ->
        case HTTPSimple.getResponseStatusCode res of
            200 -> do
                C.runConduitRes $
                    HTTP.responseBody res
                        .| case lookup "Content-Length" (HTTP.responseHeaders res) of
                            Nothing -> CL.map id
                            Just csize -> printProgress (read (B.unpack csize))
                        .| CB.sinkFileCautious destPath
                return $ Right ()
            err -> return . throwSystemError $ "Could not connect to "++url++" (got error code: "++show err++")"
    runNGLess (r :: NGLess ())

-- | Download a tar.gz file and expand it onto 'destdir'
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

printProgress :: MonadIO m => Int -> C.ConduitT B.ByteString B.ByteString m ()
printProgress csize = liftIO (mkProgressBar 40) >>= loop 0
  where
    loop !len pbar = awaitJust $ \bs -> do
            let len' = len + B.length bs
                progress = fromIntegral len' / fromIntegral csize
            pbar' <- liftIO (updateProgressBar pbar progress)
            C.yield bs
            loop len' pbar'


