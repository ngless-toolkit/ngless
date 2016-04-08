{-# LANGUAGE OverloadedStrings #-}

module Network
    ( downloadFile
    , downloadOrCopyFile
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import           Data.Conduit (($$+-), (=$=))
import Data.Conduit.Binary (sinkFile)
import System.Directory (copyFile)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP

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
    req <- HTTP.parseUrl url
    manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    res <- HTTP.http req manager

    case lookup "Content-Length" (HTTP.responseHeaders res) of
        Nothing -> throwSystemError ("HTTP Response does not contain Content-Length header" :: String)
        Just csize ->
            HTTP.responseBody res
                $$+- printProgress (read (B.unpack csize))
                =$= sinkFile destPath

printProgress :: Int -> C.Conduit B.ByteString NGLessIO B.ByteString
printProgress csize = liftIO (mkProgressBar 40) >>= loop 0
  where
    loop !len pbar = awaitJust $ \bs -> do
            let len' = len + B.length bs
                progress = fromIntegral len' / fromIntegral csize
            pbar' <- liftIO (updateProgressBar pbar progress)
            C.yield bs
            loop len' pbar'


