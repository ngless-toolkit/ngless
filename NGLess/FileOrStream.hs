{- Copyright 2016-2021 NGLess Authors
 - License: MIT
 -}

module FileOrStream
    ( FileOrStream(..)
    , asFile
    , asStream
    , asSamStream
    , origin
    ) where

import           Data.Conduit ((.|))
import qualified Data.Vector as V
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as C
import System.FilePath

import FileOrStream.Types (FileOrStream(..))
import NGLess.NGError
import Utils.Conduit (ByteLine, byteLineSinkHandle, linesVC)
import Utils.Samtools (samBamConduit)
import FileManagement

asFile :: FileOrStream -> NGLessIO FilePath
asFile (File fp) = return fp
asFile (Stream _ fp istream) =
    makeNGLTempFile "streamed_" (takeBaseNameNoExtensions fp) (takeExtensions fp) $ \hout ->
        C.runConduit $
            istream
                .| CC.concat
                .| byteLineSinkHandle hout


asStream :: FileOrStream -> (FilePath, C.ConduitT () (V.Vector ByteLine) NGLessIO ())
asStream (Stream _ fp istream) = (fp, istream)
asStream (File fp) = (fp, C.sourceFile fp .| linesVC 4096)

asSamStream (File fname) = (fname, samBamConduit fname .| linesVC 4096)
asSamStream (Stream _ fname istream) = (fname, istream)

origin :: FileOrStream -> [FilePath]
origin (File f) = [f]
origin (Stream os _ _) = concatMap origin os
