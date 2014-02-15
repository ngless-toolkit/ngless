{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 
        writeToNFiles,
        readNFiles,
        readUniqueFile
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Map as Map

import Control.Monad

import Language
import ProcessFastQ
import FileManagement

type UnrepeatedRead = Map B.ByteString [NGLessObject]

{- Could be a heuristic based on File size -}
maxBlockSize :: Int
maxBlockSize = 100000

slice :: Int -> [a] -> [[a]]
slice _  []  = []
slice s  l   = (take s l) : (slice s $ drop s l)

generateBlocks :: Int -> [a] -> [[a]]
generateBlocks bsize l = slice bsize l

writeToNFiles :: FilePath -> Int -> [NGLessObject] -> IO FilePath
writeToNFiles fname enc rs = do
    let dirPath = fname ++ "_temp/"
    createDir dirPath
    _ <- putStrLn ("Start to write N Files to: " ++ dirPath)
    forM_ (generateBlocks maxBlockSize rs) $ \x -> do
        x' <- writeReadSet (B.pack dirPath) x enc
        putStrLn x'
    _ <- putStrLn ("Wrote N Files to: " ++ dirPath)
    return dirPath

reduceReads k a b = take k (a ++ b)

readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc k d = do
    files' <- getFilesInDir d
    res <- mapM (\x -> readUniqueFile k enc (B.pack x)) files'
    return $ foldl1 (++) (elems $ unionsWith (reduceReads k) res)

readUniqueFile :: Int -> Int -> B.ByteString -> IO UnrepeatedRead
readUniqueFile k enc fname = do
    _ <- putStrLn $ "Unique -> Read: " ++ (B.unpack fname)
    (getk k . parseReadSet enc) `fmap` (readPossiblyCompressedFile fname)

getk :: Int -> [NGLessObject] -> UnrepeatedRead
getk k rs = putk k rs empty

putk :: Int -> [NGLessObject] -> UnrepeatedRead -> UnrepeatedRead
putk _ [] e = e
putk k (r:rs) e = putk k rs (put1k k r e)

put1k :: Int -> NGLessObject -> UnrepeatedRead -> UnrepeatedRead
put1k k r e = case Map.lookup (readSeq r) e of
    Just rs | length rs == k -> e
    Just rs -> Map.insert (readSeq r) (r:rs) e 
    Nothing -> Map.insert (readSeq r) [r] e 