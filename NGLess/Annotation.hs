{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Annotation
    ( intervals
    , annotate
    , getIntervalQuery
    ) where


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM
import qualified Data.IntervalMap.Interval as IM

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


annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> Maybe T.Text -> Maybe NGLessObject -> IO T.Text
annotate samFP (Just g) feats _ m = 
    printNglessLn ("annotate with GFF: " ++ (eval g) ++ (show m)) >> annotate' samFP (eval g) feats (getIntervalQuery m)  -- ignore default GFF
    where eval (NGOString n) =  getGff n
          eval _ = error ("Provided type for gff must be a NGOString.")

annotate samFP Nothing  feats g m = 
    printNglessLn ("annotate with default GFF: " ++ (show . fromJust $ g) ++ (show m)) >> 
        case g of
            Just v  -> annotate' samFP (getGff v) feats (getIntervalQuery m)       -- used default GFF
            Nothing -> error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: Maybe NGLessObject -> (IM.Interval Int -> IM.Interval Int -> Bool)
getIntervalQuery m = fromMaybe (IM.overlaps) (getMode m)

getMode m = case m of
            Nothing -> Nothing
            Just v  -> case v of
                    (NGOString "intersect") -> Just IM.overlaps
                    (NGOString "within")    -> Just IM.subsumes
                    _ -> error ("Provided type for 'mode' has to be either 'intersecting' or 'withing'")


annotate' :: FilePath -> FilePath -> Maybe NGLessObject -> (IM.Interval Int -> IM.Interval Int -> Bool) -> IO T.Text
annotate' samFp gffFp feats f = do
    gff <- L8.readFile gffFp
    sam <- L8.readFile samFp
    let imGff = intervals . filter (filterFeatures feats) . readAnnotations $ gff
    writeAnnotCount samFp $ map snd . IM.toList $ compStatsAnnot imGff sam f

compStatsAnnot :: IM.IntervalMap Int GffCount -> L8.ByteString -> (IM.Interval Int -> IM.Interval Int -> Bool) -> IM.IntervalMap Int GffCount
compStatsAnnot imGff sam f = foldl' (update f) imGff sams
   where
    sams = filter isAligned . readAlignments $ sam

update :: (IM.Interval Int -> IM.Interval Int -> Bool) -> IM.IntervalMap Int GffCount -> SamLine -> IM.IntervalMap Int GffCount
update f annots samLine = IM.mapWithKey (\k v -> update' k (interval samLine) v) annots
    where
        update' gffK samK v = case f gffK samK of 
            True  -> incCount v
            False -> v
        incCount (GffCount gId gT gC) = (GffCount gId gT (gC + 1))
        interval y = IM.ClosedInterval (samPos y) (samPos y + (cigarTLen $ samCigar y))

