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
import Control.Monad.ST

import Data.STRef
import Data.Hashable

import Language
import ProcessFastQ
import FileManagement


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
    _ <- printNglessLn $ "Start to write" ++ (show numFiles') ++ "Files to: " ++ dest
    forM_ rs $ \x -> do
        let pos = readFileN' k x
        appendFile' (fhs !! pos) (showRead enc x)
    _ <- printNglessLn $ "Wrote N Files to: " ++ dest  
    _ <- closekFileHandles fhs
    return dest


readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc k d = do
    files' <- getFilesInDir d
    res <- mapM (\x -> readUniqueFile k enc (B.pack x)) files'
    return $ concat res

readUniqueFile :: Int -> Int -> B.ByteString -> IO [NGLessObject]
readUniqueFile k enc fname = do
    _ <- printNglessLn $ "Unique -> Read: " ++ (B.unpack fname)
    (getk k . parseReadSet enc) `fmap` (readPossiblyCompressedFile fname)

--getk :: Int -> [NGLessObject] -> [NGLessObject]
getk k rs = runST $ do
    dups_ref <- newSTRef Map.empty
    putk k rs dups_ref
    res <- readSTRef dups_ref
    return $ concat . Prelude.map snd . elems $ res

--putk :: Int -> [NGLessObject] -> UnrepeatedRead
putk k rs dups_ref = do
    mapM_ (\x -> put1k k x dups_ref) rs

--put1k :: Int -> NGLessObject -> UnrepeatedRead -> UnrepeatedRead
put1k k r dups_ref = do
    mdups  <- readSTRef dups_ref
    let index = readSeq r
    case Map.lookup index mdups of
            Nothing -> writeSTRef dups_ref (Map.insert index (1,[r]) mdups)
            Just (a,b) -> case a == k of
                        True  -> return ()
                        False -> writeSTRef dups_ref (Map.insert index ((a + 1), r : b) mdups)

