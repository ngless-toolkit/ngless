{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Unique
    ( executeUnique
    , performUnique
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
import qualified Data.Conduit.List as CL
import Data.Conduit (($$), (=$=))

import FileManagement (createTempDir, openNGLTempFile)
import Data.FastQ
import NGLess
import Language
import Utils.Conduit
import Output

import qualified Data.HashTable.ST.Basic as H

(!) = V.unsafeIndex

maxTempFileSize = 100*1000*1000 -- 100MB


executeUnique :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeUnique (NGOList e) args = NGOList <$> mapM (`executeUnique` args) e
executeUnique (NGOReadSet name (ReadSet1 enc fname)) args = do
    mc <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 1) "unique argument" "max_copies" args
    newfp <- performUnique fname enc mc
    return (NGOReadSet name $ ReadSet1 enc newfp)
executeUnique expr _ = throwShouldNotOccur ("executeUnique: Cannot handle argument " ++ show expr)

performUnique fname enc mc = do
        (_,dest) <- createTempDir fname
        k    <- liftIO $ fromIntegral <$> _numFiles fname
        fhs  <- liftIO $ openKFileHandles k dest
        conduitPossiblyCompressedFile fname
            =$= linesC
            =$= fqConduitR enc
            $$ CL.mapM_ (multiplex k fhs)
        V.mapM_ (liftIO . hClose) fhs
        outputLno' DebugOutput ("Wrote N Files to: " ++ dest)
        (newfp,h) <- openNGLTempFile fname "" "fq.gz"
        sink <- asyncGzipTo h
        readNFiles enc mc dest
            =$= fqEncodeC enc
            $$ sink
        liftIO (hClose h)
        return newfp
    where
        multiplex k fhs r = liftIO $
                B.hPutStr (fhs ! hashRead k r) (fqEncode enc r)

hashRead :: Int -> ShortRead -> Int
hashRead k (ShortRead _ r _) = mod (hash r) k

readNFiles :: FastQEncoding -> Int -> FilePath -> C.Source NGLessIO ShortRead
readNFiles enc k d = do
    fs <- liftIO $ getDirectoryContents d
    let fs' = map (d </>) (filter (`notElem` [".", ".."]) fs)
    mapM_ (readUniqueFile k enc) fs'
    -- return ()

readUniqueFile :: Int -> FastQEncoding -> FilePath -> C.Source NGLessIO ShortRead
readUniqueFile k enc fname = do
    rs <- C.sourceFile fname
        =$= linesC
        =$= fqConduitR enc
        $$ CL.consume
    CL.sourceList (getk k rs)

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

