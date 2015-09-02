module Interpretation.Count
    ( executeCount
    , _filterAnnot
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8

import Language

import Data.GFF
import NGLess
import FileManagement
import Data.AnnotRes
import Utils.Utils

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOAnnotatedSet annot_fp) args = do
    let c = lookup "counts" args
        NGOInteger m = lookupWithDefault (NGOInteger 0) "min" args
        del = "\t"
    finter <- countAnnotatedSet annot_fp c m
    cont <- liftIO $ BL.readFile finter
    verbose <- boolOrTypeError "verbose arg in 'write'" $ lookupWithDefault (NGOBool False) "verbose" args
    let cont' = if verbose
                    then showGffCountDel del . readAnnotCounts $ cont
                    else showUniqIdCounts del cont
    (newfp,h) <- openNGLTempFile annot_fp "counts." "tsv"
    liftIO $ BL.hPut h cont'
    return $ NGOCounts newfp
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

countAnnotatedSet :: FilePath -> Maybe NGLessObject -> Integer -> NGLessIO FilePath
countAnnotatedSet annot_fp fs m = do
    fc <- liftIO $ readPossiblyCompressedFile annot_fp
    writeAnnotCount annot_fp $ _filterAnnot fc fs m

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
