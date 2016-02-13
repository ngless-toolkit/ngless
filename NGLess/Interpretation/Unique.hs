{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Unique
    ( executeUnique
    , _readNFiles
    , _writeToNFiles
    , _numFiles
    ) where

import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.IO.Class (liftIO)

import System.IO
import Data.Hashable
import System.FilePath.Posix
import System.Directory
import qualified Data.Vector as V

import qualified Data.ByteString as B
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($$), (=$=))

import FileManagement (createTempDir, openNGLTempFile)
import Data.FastQ
import NGLess
import Language
import Utils.Utils
import Output

import qualified Data.HashTable.ST.Basic as H

(!) = V.unsafeIndex

maxTempFileSize = 100*1000*1000 -- 100MB


writeTempFastQ :: FilePath -> [ShortRead] -> FastQEncoding -> NGLessIO FilePath
writeTempFastQ fn rs enc = do
    (newfp,h) <- openNGLTempFile fn "" "fq.gz"
    C.yieldMany rs
        =$= fqEncodeC enc
        =$= C.gzip
        $$ CB.sinkHandle h
    liftIO (hClose h)
    return newfp

executeUnique :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeUnique (NGOList e) args = NGOList <$> mapM (`executeUnique` args) e
executeUnique (NGOReadSet (ReadSet1 enc file)) args = do
        rs <- liftIO (parseFastQ enc <$> readPossiblyCompressedFile file)
        d <- _writeToNFiles file enc rs
        let NGOInteger mc = lookupWithDefault (NGOInteger 1) "max_copies" args
        uniqueCalculations' mc d --default
    where
        uniqueCalculations' :: Integer -> FilePath -> NGLessIO NGLessObject
        uniqueCalculations' numMaxOccur d = do
            fs <- liftIO $ _readNFiles enc (fromIntegral numMaxOccur) d
            nFp <- writeTempFastQ file fs enc
            return (NGOReadSet $ ReadSet1 enc nFp)
executeUnique expr _ = throwShouldNotOccur ("executeUnique: Cannot handle argument " ++ show expr)

hashRead :: Int -> ShortRead -> Int
hashRead k (ShortRead _ r _) = mod (hash r) k

_writeToNFiles :: FilePath -> FastQEncoding -> [ShortRead] -> NGLessIO FilePath
_writeToNFiles fname enc rs = do
    (_,dest) <- createTempDir fname
    liftIO $ do
        k    <- fromIntegral <$> _numFiles fname
        fhs  <- openKFileHandles k dest
        forM_ rs $ \r -> do
            let pos = hashRead k r
            B.hPutStr (fhs ! pos) (fqEncode enc r)
        V.mapM_ hClose fhs
    outputLno' DebugOutput ("Wrote N Files to: " ++ dest)
    return dest

_readNFiles :: FastQEncoding -> Int -> FilePath -> IO [ShortRead]
_readNFiles enc k d = do
    fs <- getDirectoryContents d
    let fs' = map (d </>) (filter (`notElem` [".", ".."]) fs)
    concat <$> mapM (readUniqueFile k enc) fs'

readUniqueFile :: Int -> FastQEncoding -> FilePath -> IO [ShortRead]
readUniqueFile k enc fname =
    getk k . parseFastQ enc <$> readPossiblyCompressedFile fname

getk :: Int -> [ShortRead] -> [ShortRead]
getk k rs = runST $ do
    ht <- H.new
    forM_ rs $ \r -> do
        cur <- H.lookup ht (srSequence r)
        case cur of
            Nothing -> H.insert ht (srSequence r) (1,[r])
            Just (n,ex) ->
                when (n < k) $
                    H.insert ht (srSequence r) (n+1,r:ex)
    H.foldM (\b (_,(_,a)) -> return (a ++ b)) [] ht

_numFiles :: FilePath -> IO Integer
_numFiles path = do
    fsize <- withFile path ReadMode hFileSize
    return . ceiling $ (fromInteger fsize / maxTempFileSize  :: Double)

-- Open and close file handles
openKFileHandles :: Int -> FilePath -> IO (V.Vector Handle)
openKFileHandles k dest = V.generateM k $ \n ->
        openFile (dest </> show n) AppendMode

