{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 

        writeToNFiles,
        readFromNFiles
    ) where


import Language
import ProcessFastQ

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Digest.Pure.MD5
import Data.List

import Control.Monad
import Control.DeepSeq


import System.Directory
import System.IO


hashfunc path r enc = do 
    let r' = showRead enc r
        i' = show $ md5 r'
    _ <- BL.appendFile (path ++ i' ++ ".txt") r'
    return i' 


writeToNFiles :: String -> Int -> [NGLessObject] -> IO String
writeToNFiles fname enc rs = do
    let dirPath = fname ++ "_temp/"
    createDir dirPath
    _ <- putStrLn ("Start to write N Files to: " ++ dirPath)
    forM_ rs $ \x -> do
        hashfunc dirPath x enc
    _ <- putStrLn ("Wrote N Files to: " ++ dirPath)
    return dirPath


readFile' :: String -> IO BL.ByteString
readFile' path = do
     h <- openFile path ReadMode
     s <- BL.hGetContents h
     s `deepseq` hClose h
     return s

readFromNFiles' :: Int -> String -> IO [NGLessObject]
readFromNFiles' enc fp = do 
    cont <- readFile' fp
    return $ parseReadSet enc cont
    
        
readFromNFiles :: String -> Int -> Int -> IO [NGLessObject]
readFromNFiles fp enc nMax = do
    files <- getDirectoryContents fp   
    res <- mapM (readFromNFiles' enc) $ filter (isSuffixOf "txt") files
    return $ foldl1' ((++) . take nMax) res
