{- Copyright 2016-2018 NGLess Authors
 - License: MIT
 -}

module FileOrStream
    ( FileOrStream(..)
    , asFile
    , asStream
    , asSamStream
    ) where

import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import System.FilePath

import NGLess.NGError
import Utils.Conduit
import Utils.Samtools
import FileManagement


data FileOrStream = File FilePath | Stream FilePath (C.ConduitT () ByteLine NGLessIO ())

instance Show FileOrStream where
    show (File fp) = "File " ++ fp
    show (Stream _ _) = "<STREAM>"

instance Eq FileOrStream where
    (File fp) == (File fp') = fp == fp'
    _ == _ = False

asFile :: FileOrStream -> NGLessIO FilePath
asFile (File fp) = return fp
asFile (Stream fp istream) =
    makeNGLTempFile "streamed_" (takeBaseNameNoExtensions fp) (takeExtensions fp) $ \hout ->
        C.runConduit $
            istream .| byteLineSinkHandle hout


asStream :: FileOrStream -> (FilePath, C.ConduitT () ByteLine NGLessIO ())
asStream (Stream fp istream) = (fp, istream)
asStream (File fp) = (fp, C.sourceFile fp .| linesC)

asSamStream (File fname) = (fname, samBamConduit fname .| linesC)
asSamStream (Stream fname istream) = (fname, istream)

