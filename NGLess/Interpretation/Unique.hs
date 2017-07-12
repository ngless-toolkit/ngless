{- Copyright 2013-2017 NGLess Authors
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
import Data.Conduit (($$), (=$=), (.|))

import FileManagement (createTempDir, openNGLTempFile)
import Data.FastQ
import NGLess
import Language
import Utils.Conduit
import Output

import qualified Data.HashTable.IO as H

maxTempFileSize :: Double
maxTempFileSize = 512*1000*1000 -- 512MB

executeUnique :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeUnique (NGOList e) args = NGOList <$> mapM (`executeUnique` args) e
executeUnique (NGOReadSet name (ReadSet1 enc fname)) args = do
    mc <- fromInteger <$> lookupIntegerOrScriptErrorDef (return 1) "unique argument" "max_copies" args
    newfp <- performUnique fname enc mc
    return (NGOReadSet name $ ReadSet1 enc newfp)
executeUnique expr _ = throwShouldNotOccur ("executeUnique: Cannot handle argument " ++ show expr)

performUnique :: FilePath -> FastQEncoding -> Int -> NGLessIO FilePath
performUnique fname enc mc = do
        (_,dest) <- createTempDir fname
        k <- liftIO $ do
                fsize <- withFile fname ReadMode hFileSize
                return . ceiling $! (fromInteger fsize / maxTempFileSize)
        fhs  <- liftIO $ V.generateM k $ \n ->
                            openFile (dest </> show n) AppendMode
        conduitPossiblyCompressedFile fname
            =$= linesC
            =$= fqDecodeC enc
            $$ CL.mapM_ (multiplex k fhs)
        V.mapM_ (liftIO . hClose) fhs
        outputListLno' DebugOutput ["Wrote N Files to: ", dest]
        (newfp,h) <- openNGLTempFile fname "" "fq.gz"
        readNFiles enc mc dest
            =$= fqEncodeC enc
            $$  asyncGzipTo h
        liftIO (hClose h)
        return newfp
    where
        multiplex k fhs r = liftIO $
                B.hPutStr (fhs V.! hashRead k r) (fqEncode enc r)

hashRead :: Int -> ShortRead -> Int
hashRead k (ShortRead _ r _) = mod (hash r) k


readNFiles :: FastQEncoding -> Int -> FilePath -> C.Source NGLessIO ShortRead
readNFiles enc k d = do
    fs <- liftIO $ getDirectoryContents d
    let fs' = map (d </>) (filter (`notElem` [".", ".."]) fs)
    mapM_ (readUniqueFile k enc) fs'


readUniqueFile :: Int -> FastQEncoding -> FilePath -> C.Source NGLessIO ShortRead
readUniqueFile k enc fname =
    CC.sourceFile fname
        .| linesC
        .| fqDecodeC enc
        .| filterUniqueUpTo k


filterUniqueUpTo :: Int -> C.Conduit ShortRead NGLessIO ShortRead
filterUniqueUpTo k = (liftIO H.new) >>= filterUniqueUpTo'
    where
        filterUniqueUpTo' :: H.CuckooHashTable B.ByteString Int -> C.Conduit ShortRead NGLessIO ShortRead
        filterUniqueUpTo' ht = awaitJust $ \sr -> do
            cur <- fromMaybe 0 <$> liftIO (H.lookup ht (srSequence sr))
            when (cur < k) $ do
                liftIO $ H.insert ht (srSequence sr) (cur + 1)
                C.yield sr
            filterUniqueUpTo' ht

