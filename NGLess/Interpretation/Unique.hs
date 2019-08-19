{- Copyright 2013-2019 NGLess Authors
 - License: MIT
 -}

module Interpretation.Unique
    ( executeUnique
    , performUnique
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import System.IO
import Data.Hashable
import System.FilePath
import System.Directory
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import qualified Data.ByteString as B
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))
import           Data.Conduit.Algorithms.Utils (awaitJust)
import qualified Data.Conduit.Algorithms.Async as CAlg
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)

import FileManagement (createTempDir, makeNGLTempFile)
import Data.FastQ
import NGLess
import Language
import Utils.Conduit (linesC)
import Output

import qualified Data.HashTable.IO as H

maxTempFileSize :: Double
maxTempFileSize = 512*1000*1000 -- 512MB

executeUnique :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeUnique (NGOList e) args = NGOList <$> mapM (`executeUnique` args) e
executeUnique (NGOReadSet name (ReadSet [] [FastQFilePath enc fname])) args = do
    mc <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 1) "unique argument" "max_copies" args
    newfp <- performUnique fname enc mc
    return (NGOReadSet name $ ReadSet [] [FastQFilePath enc newfp])
executeUnique expr _ = throwShouldNotOccur ("executeUnique: Cannot handle argument " ++ show expr)

performUnique :: FilePath -> FastQEncoding -> Int -> NGLessIO FilePath
performUnique fname enc mc = do
        (_,dest) <- createTempDir fname
        k <- liftIO $ do
                fsize <- withFile fname ReadMode hFileSize
                return . ceiling $! (fromInteger fsize / maxTempFileSize)
        fhs  <- liftIO $ V.generateM k $ \n ->
                            openFile (dest </> show n) AppendMode
        C.runConduitRes $
            conduitPossiblyCompressedFile fname
                .| linesC
                .| fqDecodeC enc
                .| CL.mapM_ (multiplex k fhs)
        V.mapM_ (liftIO . hClose) fhs
        outputListLno' DebugOutput ["Wrote N Files to: ", dest]
        makeNGLTempFile fname "" "fq.gz" $ \h ->
            C.runConduit $
                readNFiles enc mc dest
                    .| fqEncodeC enc
                    .| CAlg.asyncGzipTo h
    where
        multiplex k fhs r = liftIO $
                B.hPutStr (fhs V.! hashRead k r) (fqEncode enc r)

hashRead :: Int -> ShortRead -> Int
hashRead k (ShortRead _ r _) = mod (hash r) k


readNFiles :: FastQEncoding -> Int -> FilePath -> C.ConduitT () ShortRead NGLessIO ()
readNFiles enc k d = do
    fs <- liftIO $ getDirectoryContents d
    let fs' = map (d </>) (filter (`notElem` [".", ".."]) fs)
    mapM_ (readUniqueFile k enc) fs'


readUniqueFile :: Int -> FastQEncoding -> FilePath -> C.ConduitT () ShortRead NGLessIO ()
readUniqueFile k enc fname =
    CC.sourceFile fname
        .| linesC
        .| fqDecodeC enc
        .| filterUniqueUpTo k


filterUniqueUpTo :: Int -> C.ConduitT ShortRead ShortRead NGLessIO ()
filterUniqueUpTo k = liftIO H.new >>= filterUniqueUpTo'
    where
        filterUniqueUpTo' :: H.CuckooHashTable B.ByteString Int -> C.ConduitT ShortRead ShortRead NGLessIO ()
        filterUniqueUpTo' ht = awaitJust $ \sr -> do
            cur <- fromMaybe 0 <$> liftIO (H.lookup ht (srSequence sr))
            when (cur < k) $ do
                liftIO $ H.insert ht (srSequence sr) (cur + 1)
                C.yield sr
            filterUniqueUpTo' ht

