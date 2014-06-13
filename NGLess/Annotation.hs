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
import Data.Maybe (fromMaybe, fromJust)
import Data.List (foldl')

import Language
import FileManagement(printNglessLn)
import UnpackIlluminaGenomes

import Data.GFF
import Data.Sam
import Data.AnnotRes


intervals :: [GffLine] -> IM.IntervalMap Int GffCount
intervals = foldl' (insertg) IM.empty
    where
        insertg im g = IM.insert (asInterval g) (GffCount (genId g) (gffType g) 0) im
        asInterval g = IM.ClosedInterval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g


annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> Maybe T.Text -> IO T.Text
annotate samFP (Just g) feats _ = 
    printNglessLn ("annotate with GFF: " ++ (eval g)) >> annotate' samFP (eval g) feats  -- ignore default GFF
    where eval (NGOString n) =  getGff n
          eval _ = error ("Provided type for gff must be a NGOString.")
annotate samFP Nothing  feats g = 
    printNglessLn ("annotate with default GFF: " ++ (show . fromJust $ g)) >> 
        case g of
            Just v  -> annotate' samFP (getGff v) feats                            -- used default GFF
            Nothing -> error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

annotate' :: FilePath -> FilePath -> Maybe NGLessObject -> IO T.Text
annotate' samFp gffFp feats = do
    gff <- L8.readFile gffFp
    sam <- L8.readFile samFp
    let imGff = intervals . filter (filterFeatures feats) . readAnnotations $ gff
    writeAnnotCount samFp $ map snd . IM.toList $ compStatsAnnot imGff sam

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

