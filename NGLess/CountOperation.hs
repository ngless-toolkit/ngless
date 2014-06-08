module CountOperation
    (
      countAnnotatedSet
      , filterAnnot
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

import Language
import FileManagement (unCompress)

import Data.AnnotRes

countAnnotatedSet :: T.Text -> Maybe NGLessObject -> NGLessObject -> IO NGLessObject
countAnnotatedSet p f m = do
    fc <- unCompress (T.unpack p)
    print (length $ filterAnnot fc f m)
    return NGOVoid
--countAnnotatedSet p Nothing = return NGOVoid

filterAnnot :: L8.ByteString -> Maybe NGLessObject -> NGLessObject -> [GffCount]
filterAnnot fc f m = filter (filterAnnot') $ readAnnotCounts fc
    where 
      filterAnnot' c = (isMinAmount m c) && (filterCounts f c)