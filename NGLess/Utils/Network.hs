{-# LANGUAGE OverloadedStrings #-}

module Utils.Network
    ( downloadFile
    , downloadOrCopyFile
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import System.Directory (copyFile)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP

import Utils.ProgressBar

downloadOrCopyFile :: FilePath -> FilePath -> IO ()
downloadOrCopyFile src dest
    | any (`isPrefixOf` src) ["http://", "https://", "ftp://"] = downloadFile src dest
    | otherwise = copyFile src dest

downloadFile :: String -> FilePath -> IO ()
downloadFile url destPath = do
    putStrLn ("Downloading "++url)
    req <- HTTP.parseUrl url
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    runResourceT $ do
        res <- HTTP.http req manager
        case lookup "Content-Length" (HTTP.responseHeaders res) of
            Nothing -> error "HTTP Response does not contain Content-Length header"
            Just csize ->
                HTTP.responseBody res $$+-
                    printProgress (read (B.unpack csize)) =$
                    sinkFile destPath

printProgress :: Int -> Conduit B.ByteString (ResourceT IO) B.ByteString
printProgress csize = do
    pbar <- liftIO (mkProgressBar 40)
    loop 0 pbar
  where
    loop !len pbar = await >>= \case
        Nothing -> return ()
        Just bs -> do
            let len' = len + B.length bs
                progress = fromIntegral len' / fromIntegral csize
            pbar' <- liftIO (updateProgressBar pbar progress)
            yield bs
            loop len' pbar'


