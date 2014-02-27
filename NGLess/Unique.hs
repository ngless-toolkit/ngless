{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 
        readNFiles,
        readUniqueFile,
        writeToNFiles,
        numFiles
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Map as Map

import Control.Monad

import Data.Hashable

import Language
import ProcessFastQ
import FileManagement

type UnrepeatedRead = Map B.ByteString [NGLessObject]


readFileN :: Int -> NGLessObject -> Int
readFileN k (NGOShortRead _ r _) = 
    mod (hash r) k 

readFileN _ err = error ("readFileN should receive a NGOShortRead, but received a " ++ (show err))


readFileN' :: Int -> NGLessObject -> Int
readFileN' k r = readFileN k r

writeToNFiles :: FilePath -> Int -> [NGLessObject] -> IO FilePath
writeToNFiles fname enc rs = do
    dest <- createDir fname
    numFiles' <- numFiles fname
    let k = fromIntegral numFiles'
    fhs <- openKFileHandles k dest    
    _ <- putStrLn ("Start to write" ++ (show numFiles') ++ "Files to: " ++ dest)
    forM_ rs $ \x -> do
        let pos = readFileN' k x
        appendFile' (fhs !! pos) (showRead enc x)
    _ <- putStrLn ("Wrote N Files to: " ++ dest)
    _ <- closekFileHandles fhs
    return dest


readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc k d = do
    files' <- getFilesInDir d
    res <- mapM (\x -> readUniqueFile k enc (B.pack x)) files'
    return $ concat res

readUniqueFile :: Int -> Int -> B.ByteString -> IO [NGLessObject]
readUniqueFile k enc fname = do
    _ <- putStrLn $ "Unique -> Read: " ++ (B.unpack fname)
    (getk k . parseReadSet enc) `fmap` (readPossiblyCompressedFile fname)

getk :: Int -> [NGLessObject] -> [NGLessObject]
getk k rs = concat . elems $ putk k rs empty

putk :: Int -> [NGLessObject] -> UnrepeatedRead -> UnrepeatedRead
putk _ [] e = e
putk k (r:rs) e = putk k rs (put1k k r e)

put1k :: Int -> NGLessObject -> UnrepeatedRead -> UnrepeatedRead
put1k k r e = case Map.lookup (readSeq r) e of
    Just rs | length rs == k -> e
    Just rs -> Map.insert (readSeq r) (r:rs) e 
    Nothing -> Map.insert (readSeq r) [r] e 