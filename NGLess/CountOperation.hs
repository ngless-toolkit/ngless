module CountOperation
    (
      countAnnotatedSet
      , filterAnnot
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Maybe

import Language
import FileManagement (unCompress)

import Data.AnnotRes

countAnnotatedSet :: T.Text -> Maybe NGLessObject -> IO NGLessObject
countAnnotatedSet p f = do
    fc <- unCompress (T.unpack p)
    print (length $ filterAnnot fc f)
    return NGOVoid
--countAnnotatedSet p Nothing = return NGOVoid

filterAnnot :: L8.ByteString -> Maybe NGLessObject -> [GffCount]
filterAnnot fc f = filter (filterCounts f) $ readAnnotCounts fc