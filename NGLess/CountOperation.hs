module CountOperation
    (
      countAnnotatedSet
      , filterAnnot
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

import Language
import FileManagement (readPossiblyCompressedFile)

import Data.AnnotRes

countAnnotatedSet :: T.Text -> Maybe NGLessObject -> NGLessObject -> IO T.Text
countAnnotatedSet p f m = do
    fc <- readPossiblyCompressedFile p'
    writeAnnotCount p' $ filterAnnot fc f m
   where p' = T.unpack p
--countAnnotatedSet p Nothing = return NGOVoid

filterAnnot :: L8.ByteString -> Maybe NGLessObject -> NGLessObject -> [GffCount]
filterAnnot fc f m = filter (filterAnnot') $ readAnnotCounts fc
    where 
      filterAnnot' c = (isMinAmount m c) && (filterCounts f c)
