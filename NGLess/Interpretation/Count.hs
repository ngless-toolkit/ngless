module Interpretation.Count
    ( executeCount
    , _filterAnnot
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as L8

import Language

import Data.GFF
import NGLess
import Data.AnnotRes
import Utils.Utils

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOAnnotatedSet p) args = do
    let c = lookup "counts" args
        NGOInteger m = lookupWithDefault (NGOInteger 0) "min" args
    NGOAnnotatedSet <$> countAnnotatedSet p c m
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

countAnnotatedSet :: FilePath -> Maybe NGLessObject -> Integer -> NGLessIO FilePath
countAnnotatedSet p fs m = do
    fc <- liftIO $ readPossiblyCompressedFile p
    writeAnnotCount p $ _filterAnnot fc fs m

_filterAnnot :: L8.ByteString -> Maybe NGLessObject -> Integer -> [GffCount]
_filterAnnot fc fs minval = filter _filterAnnot' $ readAnnotCounts fc
    where
        _filterAnnot' c = hasMinCount c && filterCounts fs' c
        hasMinCount :: GffCount ->  Bool
        hasMinCount = (>= minval) . toInteger . annotCount

        filterCounts Nothing _ = True
        filterCounts (Just allowed) c = annotType c `elem` allowed

        fs' = extract <$> fs
        extract (NGOList s) = map extract1 s
        extract e = error ("type checker should have caught this (filterAnnot.extract receieved '" ++ show e)

        extract1 (NGOSymbol "gene") = GffGene
        extract1 (NGOSymbol "exon") = GffExon
        extract1 (NGOSymbol "cds" ) = GffCDS
        extract1 (NGOSymbol "CDS" ) = GffCDS
        extract1 (NGOSymbol s)      = GffOther (T.encodeUtf8 s)
        extract1 e = error ("type checker should have caught this (filterAnnot.extract receieved '" ++ show e)
