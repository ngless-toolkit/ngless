{-# LANGUAGE OverloadedStrings #-}

module Utils.Network
    ( downloadFile
    ) where

import Control.Monad.Error (liftIO)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Conduit as HTTP

import Utils.ProgressBar

downloadFile :: String -> FilePath -> IO ()
downloadFile url destPath = do
    req <- HTTP.parseUrl url
    manager <- HTTP.newManager HTTP.conduitManagerSettings
    runResourceT $ do
        res <- HTTP.http req manager
        case lookup "Content-Length" (HTTP.responseHeaders res) of
            Nothing -> error "HTTP Response does not contain Content-Length header"
            Just csize -> do
                HTTP.responseBody res $$+-
                    printProgress (read (B.unpack csize)) =$
                    sinkFile destPath

printProgress :: Int -> Conduit B.ByteString (ResourceT IO) B.ByteString
printProgress csize = do
    pbar <- liftIO (mkProgressBar 40)
    loop 0 pbar
  where
    loop len pbar = await >>= maybe (return ()) (\bs -> do
            let len' = len + B.length bs
                progress = (fromIntegral len' / fromIntegral csize)
            pbar' <- liftIO (updateProgressBar pbar progress)
            yield bs
            loop len' pbar'
            )


