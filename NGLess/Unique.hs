{-# LANGUAGE OverloadedStrings #-}

module Unique
    ( 

        writeToNFiles,
        readFromNFiles
    ) where


import Language
import ProcessFastQ

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

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

readFromNFiles' :: FilePath -> Int -> Int -> String -> IO ()
readFromNFiles' newfp enc nMax fp = do 
    cont <- readFile' fp
    res' <- mapM_ (BL.appendFile newfp . showRead enc) (take nMax $ parseReadSet enc cont)
    return res'
    
        
readFromNFiles :: B.ByteString -> String -> Int -> Int -> IO FilePath
readFromNFiles file fp enc nMax = do
    files <- getDirectoryContents fp
    newfp <- getTFilePath file
    forM_ (filter (isSuffixOf "txt") files) $ \x -> do
        readFromNFiles' newfp enc nMax x
    return newfp
