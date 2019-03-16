{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.FastQ.Utils
    ( concatenateFQs
    ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((.|))
import System.IO (hClose)
import Control.Monad
import Control.Monad.Except

import Data.List (isSuffixOf)

import NGLess.NGError

import FileManagement
import Utils.Conduit (linesC)
import Data.FastQ
import Data.Conduit.Algorithms.Async (asyncGzipTo, conduitPossiblyCompressedFile)

concatenateFQs :: [FastQFilePath] -> NGLessIO FastQFilePath
concatenateFQs [] = throwShouldNotOccur "Empty argument to concatenateFQs"
concatenateFQs [f] = return f
concatenateFQs (FastQFilePath enc fp:rest) = do
    (fres, h) <- openNGLTempFile "concatenate" fp "fq.gz"
    let catTo f enc'
            | enc /= enc' =
                            conduitPossiblyCompressedFile f
                                .| linesC
                                .| fqDecodeC enc'
                                .| fqEncodeC enc
                                .| asyncGzipTo h
            | ".gz" `isSuffixOf` f = CB.sourceFile f .| CB.sinkHandle h
            | otherwise = conduitPossiblyCompressedFile f .| asyncGzipTo h
    C.runConduitRes $ catTo fp enc
    forM_ rest $ \(FastQFilePath enc' f') ->
        C.runConduitRes (catTo f' enc')
    liftIO $ hClose h
    return $ FastQFilePath enc fres


