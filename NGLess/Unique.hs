{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 
        readNFiles,
        readUniqueFile,
        writeToNFiles,
        numFiles,
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map as Map

import Control.Monad
import Control.Monad.ST

import Data.STRef
import Data.Hashable

import Language
import ProcessFastQ
import FileManagement

import Data.DefaultValues


readFileN :: Int -> NGLessObject -> Int
readFileN k (NGOShortRead _ r _) = 
    mod (hash r) k 

readFileN _ err = error ("readFileN should receive a NGOShortRead, but received a " ++ (show err))


writeToNFiles :: FilePath -> Int -> [NGLessObject] -> IO FilePath
writeToNFiles fname enc rs = do
    dest <- createDir fname
    k    <- numFiles fname >>= return . fromIntegral 
    fhs  <- openKFileHandles k dest    
    forM_ rs $ \x -> do
        let pos = readFileN k x
        BL.hPutStrLn (fhs !! pos) (showRead enc x)
    _ <- do
        printNglessLn $ "Wrote N Files to: " ++ dest 
        closekFileHandles fhs
    return dest


readNFiles :: Int -> Int -> FilePath -> IO [NGLessObject]
readNFiles enc k d = getFilesInDir d >>= mapM (\x -> readUniqueFile k enc x) >>= return . concat

readUniqueFile :: Int -> Int -> FilePath -> IO [NGLessObject]
readUniqueFile k enc fname = do
    _ <- printNglessLn $ "Unique -> Read: " ++ fname
    (getk k . parseReadSet enc) `fmap` (unCompress fname)

getk :: Int -> [NGLessObject] -> [NGLessObject]
getk k rs = runST $ do
    dups_ref <- newSTRef Map.empty
    putk k rs dups_ref
    res <- readSTRef dups_ref
    return $ Map.fold (\(_,a) b -> a ++ b) [] res

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

