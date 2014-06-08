{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Annotation
    ( intervals
    , annotate
    ) where


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM

import Language

import Data.Maybe (fromMaybe)
import Data.List (foldl')

import Data.GFF
import Data.Sam
import Data.AnnotRes


intervals :: [GffLine] -> IM.IntervalMap Int GffCount
intervals = foldl' (insertg) IM.empty
    where
        insertg im g = IM.insert (asInterval g) (GffCount (genId g) (gffType g) 0) im
        asInterval g = IM.ClosedInterval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g


annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> IO T.Text
annotate samFP (Just gffFP) feats = annotate' samFP gffFP feats
annotate samFP Nothing feats = annotate' samFP (NGOString "to-be-determided") feats


annotate' :: FilePath -> NGLessObject -> Maybe NGLessObject -> IO T.Text
annotate' samFp (NGOString gffFp) feats = do
    gff <- L8.readFile (T.unpack gffFp)
    sam <- L8.readFile samFp
    let imGff = intervals . filter (filterFeatures feats) . readAnnotations $ gff
    writeAnnotCount samFp $ map snd . IM.toList $ compStatsAnnot imGff sam

annotate' _ s _ = error ("Should be a NGOString but is a: " ++ (show s))


compStatsAnnot :: IM.IntervalMap Int GffCount -> L8.ByteString -> IM.IntervalMap Int GffCount
compStatsAnnot imGff sam = foldl' update imGff sams
   where
    sams = filter isAligned . readAlignments $ sam

update :: IM.IntervalMap Int GffCount -> SamLine -> IM.IntervalMap Int GffCount
update annots samLine = do
    let res = IM.intersecting annots (interval samLine)
    foldl' (\im (k,_) -> IM.adjust incCount k im) annots res
  where
    incCount (GffCount gId gT gC) = (GffCount gId gT (gC + 1))
    interval y = IM.ClosedInterval (samPos y) (samPos y + (cigarTLen $ samCigar y))

