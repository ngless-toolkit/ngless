{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.FastQ.Utils
    ( concatenateFQs
    ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((.|))
import           Control.Monad (forM_)

import Data.List (isSuffixOf)

import NGLess.NGError

import FileManagement (makeNGLTempFile)
import Utils.Conduit (linesC)
import Data.FastQ (FastQFilePath(..), fqDecodeC, fqEncodeC)
import Data.Conduit.Algorithms.Async (asyncGzipTo, conduitPossiblyCompressedFile)

concatenateFQs :: [FastQFilePath] -> NGLessIO FastQFilePath
concatenateFQs [] = throwShouldNotOccur "Empty argument to concatenateFQs"
concatenateFQs [f] = return f
concatenateFQs (FastQFilePath enc fp:rest) = do
    fres <- makeNGLTempFile fp "concatenate" "fq.gz" $ \hout -> do
        let catTo f enc'
                | enc /= enc' =
                                conduitPossiblyCompressedFile f
                                    .| linesC
                                    .| fqDecodeC enc'
                                    .| fqEncodeC enc
                                    .| asyncGzipTo hout
                | ".gz" `isSuffixOf` f = CB.sourceFile f .| CB.sinkHandle hout
                | otherwise = conduitPossiblyCompressedFile f .| asyncGzipTo hout
        C.runConduitRes $ catTo fp enc
        forM_ rest $ \(FastQFilePath enc' f') ->
            C.runConduitRes (catTo f' enc')
    return $ FastQFilePath enc fres


