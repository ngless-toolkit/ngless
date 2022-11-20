{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.FastQ.Utils
    ( concatenateFQs
    , interleaveFQs
    ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import           Data.Conduit ((.|))
import           Control.Monad (forM_)
import Data.Vector qualified as V
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty qualified as NE


import Data.List (isSuffixOf)

import NGLess.NGError (NGLessIO, throwShouldNotOccur)

import FileManagement (makeNGLTempFile)
import Utils.Conduit (linesVC, unwrapByteLine, ByteLine(..), zipSource2)
import Data.FastQ (ReadSet(..), FastQFilePath(..), fqDecodeVC, fqEncodeC)
import Data.Conduit.Algorithms.Async (asyncGzipTo, conduitPossiblyCompressedFile)
import Output (traceStatus)

concatenateFQs :: NE.NonEmpty FastQFilePath -> NGLessIO FastQFilePath
concatenateFQs (f NE.:| []) = return f
concatenateFQs (FastQFilePath enc fp NE.:| rest) = do
    fres <- makeNGLTempFile fp "concatenate" "fq.gz" $ \hout -> do
        let catTo f enc'
                | enc /= enc' =
                    conduitPossiblyCompressedFile f
                        .| linesVC 4096
                        .| fqDecodeVC f 0 enc'
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
                ((conduitPossiblyCompressedFile f0 .| linesVC 4096) `zipSource2` (conduitPossiblyCompressedFile f1 .| linesVC 4096))
                .| C.awaitForever (\(v0,v1) -> do
                            if V.length v0 == V.length v1
                                then C.yield (interleaveV v0 v1)
                                else throwShouldNotOccur ("interleavePair: mismatched lengths: " ++ show (V.length v0, V.length v1)))
        interleaveV :: V.Vector ByteLine -> V.Vector ByteLine -> B.ByteString
        interleaveV v0 v1 =
            B8.unlines $ V.toList $ V.map unwrapByteLine $ V.generate (V.length v0 + V.length v1) $ \i ->
                let (seqi, si) = i `divMod` 4
                in if even seqi
                    then v0 V.! (seqi * 2 + si)
                    else v1 V.! ((seqi - 1) * 2 + si)

