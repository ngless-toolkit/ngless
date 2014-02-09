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

import System.Directory
import System.IO


hashfunc path r enc = do 
    let r' = showRead enc r
        d' =  md5 r'
        i' = show d'
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


         

readFromNFiles :: String -> Int -> Int -> IO [NGLessObject]
readFromNFiles fp enc nMax = do
    files <- getDirectoryContents fp
    let files' = map ((++) fp) $ filter (isSuffixOf "txt") files   
    res <- mapM (\path -> withFile path ReadMode $ \hnd -> do
                    c <- BL.hGetContents hnd
                    return (take nMax $ parseReadSet enc c)) files'
    return $ foldl1 (++) res