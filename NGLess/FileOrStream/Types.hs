{- Copyright 2016-2022 NGLess Authors
 - License: MIT
 -}

module FileOrStream.Types
    ( FileOrStream(..)
    ) where

import qualified Data.Vector as V
import qualified Data.Conduit as C

import NGLess.NGError
import Utils.Conduit

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

