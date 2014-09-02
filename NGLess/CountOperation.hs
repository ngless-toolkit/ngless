module CountOperation
    (
      countAnnotatedSet
      , filterAnnot
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

import Language
import FileManagement (readPossiblyCompressedFile)

import Data.GFF
import Data.AnnotRes

countAnnotatedSet :: FilePath -> Maybe NGLessObject -> NGLessObject -> IO FilePath
countAnnotatedSet p f m = do
    fc <- readPossiblyCompressedFile p
    writeAnnotCount p $ filterAnnot fc f m
--countAnnotatedSet p Nothing = return NGOVoid

filterAnnot :: L8.ByteString -> Maybe NGLessObject -> NGLessObject -> [GffCount]
filterAnnot fc f m = filter (filterAnnot') $ readAnnotCounts fc
    where
      filterAnnot' c = (isMinAmount m c) && (filterCounts f c)


--- Quite similar to the filterFeatures in GFF but access different field name.
filterCounts :: Maybe NGLessObject -> GffCount -> Bool
filterCounts feats annotL = maybe True (fFeat annotL) feats
  where
        fFeat g (NGOList f) = foldl (\a b -> a || b) False (map (filterCounts' g) f)
        fFeat _ err = error("Type should be NGOList but received: " ++ (show err))

filterCounts' :: GffCount -> NGLessObject -> Bool
filterCounts' g (NGOSymbol "gene") = (==GffGene) . annotType $ g
filterCounts' g (NGOSymbol "exon") = (==GffExon) . annotType $ g
filterCounts' g (NGOSymbol "cds" ) = (==GffCDS) . annotType  $ g
filterCounts' g (NGOSymbol "CDS" ) = (==GffCDS) . annotType  $ g
filterCounts' g (NGOSymbol s) = (show . annotType $ g) == (T.unpack s)
filterCounts' _ err = error ("Type should be NGOList but received: " ++ (show err))

isMinAmount :: NGLessObject -> GffCount ->  Bool
isMinAmount (NGOInteger l) g = (toInteger $ annotCount g) >= l
isMinAmount err _ = error ("Type should be NGOInteger but received: " ++ (show err))
