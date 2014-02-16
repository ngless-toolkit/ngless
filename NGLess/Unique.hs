{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 
        writeToNFiles,
        readNFiles,
        readUniqueFile
    ) where

import qualified Data.ByteString.Char8 as B

import Data.Map as Map
import Data.List

import Control.Monad

import Data.Hashable

import Language
import ProcessFastQ
import FileManagement

type UnrepeatedRead = Map B.ByteString [NGLessObject]

{- Should be a heuristic based on File size -}
numFiles :: Int
numFiles = 6

readFileN :: NGLessObject -> Int
readFileN (NGOShortRead _ r _) = mod (hash r) numFiles
readFileN err = error ("readFileN should receive a NGOShortRead, but received a " ++ (show err))

changeNthElement :: Int -> (a -> a) -> [a] -> [a]
changeNthElement idx transform list =  case splitAt idx list of
                    (front, element:back) -> front ++ transform element : back
                    _ -> list --Invalid index position.

slice :: [NGLessObject] -> [[NGLessObject]]
slice []     = replicate numFiles []
slice (x:xs) = do
    let xs' = slice xs
        pos = (readFileN x)
    changeNthElement pos ((:) x) xs'

writeToNFiles :: FilePath -> Int -> [NGLessObject] -> IO FilePath
writeToNFiles fname enc rs = do
    let dirPath = fname ++ "_temp/"
    createDir dirPath
    _ <- putStrLn ("Start to write N Files to: " ++ dirPath)
    forM_ (slice rs) $ \x -> do
        x' <- writeReadSet (B.pack dirPath) x enc
        putStrLn x'
 --       appendFile' (dirPath ++ (readFileN x)) (BL.append (showRead enc x) "\n")
    _ <- putStrLn ("Wrote N Files to: " ++ dirPath)
    return dirPath

readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc k d = do
    files' <- getFilesInDir d
    res <- mapM (\x -> readUniqueFile k enc (B.pack x)) files'
    return $ foldl1' (++) res

readUniqueFile :: Int -> Int -> B.ByteString -> IO [NGLessObject]
readUniqueFile k enc fname = do
    _ <- putStrLn $ "Unique -> Read: " ++ (B.unpack fname)
    (getk k . parseReadSet enc) `fmap` (readPossiblyCompressedFile fname)

getk :: Int -> [NGLessObject] -> [NGLessObject]
getk k rs = foldl1' (++) . elems $ putk k rs empty

putk :: Int -> [NGLessObject] -> UnrepeatedRead -> UnrepeatedRead
putk _ [] e = e
putk k (r:rs) e = putk k rs (put1k k r e)

put1k :: Int -> NGLessObject -> UnrepeatedRead -> UnrepeatedRead
put1k k r e = case Map.lookup (readSeq r) e of
    Just rs | length rs == k -> e
    Just rs -> Map.insert (readSeq r) (r:rs) e 
    Nothing -> Map.insert (readSeq r) [r] e 