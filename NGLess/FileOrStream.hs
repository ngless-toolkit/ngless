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

import NGLess.NGError
import Utils.Conduit
import Utils.Samtools
import FileManagement

{- FileOrStream is either a file or a stream, with the following extra information
 -  1. A filename
 -  2. The origin of the data
 -
 -  If it is a file, then both (1) and (2) are just the actual filename, but if
 -  it's a stream then (1) is the fake name of the file and (2) points
 -  (potentially removed) to where the original data is on disk. It is
 -  necessary to keep track of this to implement GC correctly.
-}
data FileOrStream = File FilePath
                    | Stream [FileOrStream] -- ^ the origin of stream
                                FilePath -- ^ the "filename"
                                (C.ConduitT () (V.Vector ByteLine) NGLessIO ())

instance Show FileOrStream where
    show (File fp) = "File " ++ fp
    show Stream{} = "<STREAM>"

instance Eq FileOrStream where
    (File fp) == (File fp') = fp == fp'
    _ == _ = False

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
