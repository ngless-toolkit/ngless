{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( readNFiles
    , readUniqueFile
    , writeToNFiles
    , numFiles
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map as Map

import Control.Monad
import Control.Monad.ST

import System.IO
import Data.STRef
import Data.Hashable
import System.FilePath.Posix


import FileManagement (createDir, getFilesInDir, readPossiblyCompressedFile)
import Data.FastQ
import Configuration

hashRead :: Int -> ShortRead -> Int
hashRead k (ShortRead _ r _) = mod (hash r) k

writeToNFiles :: FilePath -> FastQEncoding -> [ShortRead] -> IO FilePath
writeToNFiles fname enc rs = do
    dest <- createDir fname
    k    <- numFiles fname >>= return . fromIntegral 
    fhs  <- openKFileHandles k dest    
    forM_ rs $ \x -> do
        let pos = hashRead k x
        BL.hPutStr (fhs !! pos) (asFastQ enc [x])
    _ <- do
        printNglessLn $ "Wrote N Files to: " ++ dest 
        mapM_ hClose fhs
    return dest


readNFiles :: FastQEncoding -> Int -> FilePath -> IO [ShortRead]
readNFiles enc k d = getFilesInDir d >>= mapM (\x -> readUniqueFile k enc x) >>= return . concat

readUniqueFile :: Int -> FastQEncoding -> FilePath -> IO [ShortRead]
readUniqueFile k enc fname =
    (getk k . parseFastQ enc) `fmap` (readPossiblyCompressedFile fname)

getk :: Int -> [ShortRead] -> [ShortRead]
getk k rs = runST $ do
    dups_ref <- newSTRef Map.empty
    mapM_ (\x -> put1k k x dups_ref) rs
    res <- readSTRef dups_ref
    return $ Map.fold (\(_,a) b -> a ++ b) [] res


--put1k :: Int -> ShortRead -> UnrepeatedRead -> UnrepeatedRead
put1k k r dups_ref = do
    mdups  <- readSTRef dups_ref
    let index = srSequence r
    case Map.lookup index mdups of
        Nothing -> writeSTRef dups_ref (Map.insert index (1,[r]) mdups)
        Just (a,b) -> when (a /= k) (writeSTRef dups_ref (Map.insert index ((a + 1), r : b) mdups))

numFiles :: FilePath -> IO Integer
numFiles path = do
    fsize <- withFile path ReadMode hFileSize
    tmpSize <- maxTempFileSize
    return $ ceiling $ ((fromInteger fsize) / tmpSize  :: Double)


-- Open and close file handles
openKFileHandles :: Int -> FilePath -> IO [Handle]
openKFileHandles k dest =
    forM [0..k - 1] $ \n -> do
        openFile (dest </> show n) AppendMode

