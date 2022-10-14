{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.FastQ.Utils
    ( concatenateFQs
    , interleaveFQs
    ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import           Data.Conduit ((.|))
import           Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)


import Data.List (isSuffixOf)

import NGLess.NGError (NGLessIO, throwShouldNotOccur)

import FileManagement (makeNGLTempFile)
import Utils.Conduit (linesC, unwrapByteLine)
import Data.FastQ (ReadSet(..), FastQFilePath(..), fqDecodeC, fqEncodeC)
import Data.Conduit.Algorithms.Async (asyncGzipTo, conduitPossiblyCompressedFile)
import Output (traceStatus)

concatenateFQs :: [FastQFilePath] -> NGLessIO FastQFilePath
concatenateFQs [] = throwShouldNotOccur "Empty argument to concatenateFQs"
concatenateFQs [f] = return f
concatenateFQs (FastQFilePath enc fp:rest) = do
    fres <- makeNGLTempFile fp "concatenate" "fq.gz" $ \hout -> do
        let catTo f enc'
                | enc /= enc' =
                    conduitPossiblyCompressedFile f
                        .| linesC
                        .| fqDecodeC f enc'
                        .| fqEncodeC enc
                        .| asyncGzipTo hout
                | ".gz" `isSuffixOf` f = CB.sourceFile f .| CB.sinkHandle hout
                | otherwise = conduitPossiblyCompressedFile f .| asyncGzipTo hout
        C.runConduitRes $ catTo fp enc
        forM_ rest $ \(FastQFilePath enc' f') ->
            C.runConduitRes (catTo f' enc')
    return $ FastQFilePath enc fres


interleaveFQs :: ReadSet -> C.ConduitT () B.ByteString NGLessIO ()
interleaveFQs (ReadSet pairs singletons) = do
            sequence_ [
                ((lift $ traceStatus ("Reading files '" ++ f0 ++ "' and '" ++ f1 ++ "'"))
                  >> interleavePair f0 f1) | (FastQFilePath _ f0, FastQFilePath _ f1) <- pairs]
            sequence_ [
                ((lift $ traceStatus ("Reading file '" ++ f ++ "'"))
                    >> conduitPossiblyCompressedFile f) | FastQFilePath _ f <- singletons]
    where
        interleavePair :: FilePath -> FilePath -> C.ConduitT () B.ByteString NGLessIO ()
        interleavePair f0 f1 =
                ((conduitPossiblyCompressedFile f0 .| linesC .| CL.chunksOf 4) `zipSources` (conduitPossiblyCompressedFile f1 .| linesC .| CL.chunksOf 4))
                .| C.awaitForever (\(r0,r1) -> C.yield (ul r0) >> C.yield (ul r1))
        zipSources a b = C.getZipSource ((,) <$> C.ZipSource a <*> C.ZipSource b)
        ul = B8.unlines . map unwrapByteLine

