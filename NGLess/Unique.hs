{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 
        writeToNFiles,
        readNFiles
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Map as Map
import Data.List

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
        newfp <- generateTempFilePath dirPath fname
        writeReadSet (B.pack newfp) x enc
    _ <- putStrLn ("Wrote N Files to: " ++ dirPath)
    return dirPath

readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc numMaxOccur file = do
    files' <- getFilesInDir file
    res <- mapM (\x -> readUniqueFile numMaxOccur enc (B.pack x)) files'
    return $ foldl1 (++) (elems $ unions res)

readUniqueFile :: Int -> Int -> B.ByteString -> IO UnrepeatedRead
readUniqueFile k enc fname = (getk k . parseReadSet enc) `fmap` (readPossiblyCompressedFile fname)

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