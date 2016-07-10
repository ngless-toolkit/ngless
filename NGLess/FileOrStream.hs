{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module FileOrStream
    ( FileOrStream(..)
    , asFile
    , asStream
    ) where

import           Control.Monad.Writer
import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import System.IO
import System.FilePath

import NGLess.NGError
import Utils.Conduit
import FileManagement


data FileOrStream = File FilePath | Stream FilePath (C.Source NGLessIO ByteLine)

instance Show FileOrStream where
    show (File fp) = "File " ++ fp
    show (Stream _ _) = "<STREAM>"

instance Eq FileOrStream where
    (File fp) == (File fp') = fp == fp'
    _ == _ = False

asFile :: FileOrStream -> NGLessIO FilePath
asFile (File fp) = return fp
asFile (Stream fp istream) = do
    (newfp,hout) <- openNGLTempFile "streamed_" (takeBaseNameNoExtensions fp) (takeExtensions fp)
    istream $$ byteLineSinkHandle hout
    liftIO (hClose hout)
    return newfp


asStream :: FileOrStream -> (FilePath, C.Source NGLessIO ByteLine)
asStream (Stream fp istream) = (fp, istream)
asStream (File fp) = (fp, C.sourceFile fp =$= linesC)


