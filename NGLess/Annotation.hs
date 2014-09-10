{-# LANGUAGE OverloadedStrings #-}

module Annotation
    ( AnnotationIntersectionMode(..)
    , annotate
    , _intersection_strict
    , _intersection_non_empty
    , _filterFeatures
    , _allSameId
    ) where


import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M

import Data.Maybe (fromJust)
import Data.Foldable(foldl')
import Control.DeepSeq

import FileManagement(readPossiblyCompressedFile)
import ReferenceDatabases
import Configuration

import Data.GFF
import Data.Sam (SamLine(..), isAligned, isPositive, readAlignments)
import Data.AnnotRes

type AnnotationMap = M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount]))

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

annotate :: FilePath -> Maybe FilePath -> Maybe [String] -> Maybe T.Text -> AnnotationIntersectionMode -> Bool -> Bool -> IO FilePath
annotate samFP (Just g) feats _ m a s = do
    printNglessLn (concat ["annotate with GFF: ", g])
    annotate' samFP g feats (getIntervalQuery m) a s  -- ignore default GFF
annotate samFP Nothing feats dDs m a s =
    printNglessLn (concat ["annotate with default GFF: ", show . fromJust $ dDs]) >>
        case dDs of
            Just v  -> do
                basedir <- ensureDataPresent (T.unpack v)
                annotate' samFP (getGff basedir) feats (getIntervalQuery m) a s   -- used default GFF
            Nothing -> error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: AnnotationIntersectionMode -> (IM.IntervalMap Int [GffCount] -> (Int, Int) -> IM.IntervalMap Int [GffCount])
getIntervalQuery IntersectUnion = union
getIntervalQuery IntersectStrict = _intersection_strict
getIntervalQuery IntersectNonEmpty = _intersection_non_empty


annotate' :: FilePath -> FilePath -> Maybe [String] -> (IM.IntervalMap Int [GffCount] -> (Int,Int) -> IM.IntervalMap Int [GffCount]) -> Bool -> Bool -> IO FilePath
annotate' samFp gffFp feats a f s = do
        gffC <- readPossiblyCompressedFile gffFp >>= return . intervals . filter (_filterFeatures feats) . readAnnotations
        samC <- readPossiblyCompressedFile samFp >>= return . filter isAligned . readAlignments 
        let res = calculateAnnotation gffC samC
        writeAnnotCount samFp (toGffM . concat . map (M.elems) . M.elems $ res)
    where
        calculateAnnotation :: AnnotationMap -> [SamLine] -> AnnotationMap
        calculateAnnotation aMap sam = aMap `deepseq` compStatsAnnot aMap sam f a s


toGffM :: [IM.IntervalMap Int [GffCount]] -> [GffCount]
toGffM = concat . concat . map IM.elems


compStatsAnnot ::  AnnotationMap -> [SamLine] -> Bool -> (IM.IntervalMap Int [GffCount] -> (Int,Int) -> IM.IntervalMap Int [GffCount]) -> Bool -> AnnotationMap
compStatsAnnot imGff sam a f s = foldl iterSam imGff sam
    where
      iterSam im y = M.map (M.alter alterCounts k) im
        where
            alterCounts Nothing = Nothing
            alterCounts (Just v) = Just $ modeAnnotation f a v y s
            k = samRName y


modeAnnotation :: (IM.IntervalMap Int [GffCount] -> (Int,Int) -> IM.IntervalMap Int [GffCount]) -> Bool -> IM.IntervalMap Int [GffCount] -> SamLine -> Bool -> IM.IntervalMap Int [GffCount]
modeAnnotation f a im y s = countsAmbiguity a ((filterStrand s asStrand) . (f im) $ (sStart, sEnd)) im
  where
    sStart = samPos y
    sEnd   = sStart + (samCigLen y) - 1
    asStrand = if isPositive y then GffPosStrand else GffNegStrand


filterStrand :: Bool -> GffStrand -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
filterStrand True  s m =  IM.filter (not . null) . IM.map (filterByStrand s) $ m
filterStrand False _ m = m


countsAmbiguity :: Bool -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
countsAmbiguity True toU imR = uCounts toU imR
countsAmbiguity False toU imR = case IM.null toU of
        True  -> imR -- no_feature
        False -> case _allSameId toU of
            True  -> uCounts (IM.fromList . take 1 . IM.toList $ toU) imR -- same feature multiple times. increase that feature ONCE.
            False -> imR -- ambiguous


uCounts :: IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
uCounts keys im = IM.foldlWithKey (\res k _ -> IM.adjust (incCount) k res) im keys
    where
        incCount []     = []
        incCount (x:rs) = incCount' x : rs
        incCount' (GffCount gId gT !gC gS) = (GffCount gId gT (gC + 1) gS)



--- Diferent modes

union :: IM.IntervalMap Int [GffCount] -> (Int, Int) -> IM.IntervalMap Int [GffCount]
union im (sS, sE) = IM.fromList $ IM.intersecting im intv
    where
        intv = IM.ClosedInterval sS sE

_intersection_strict :: IM.IntervalMap Int [GffCount] -> (Int, Int) -> IM.IntervalMap Int [GffCount]
_intersection_strict im (sS, sE) = intersection' im'
    where 
        im' = map (IM.fromList . (IM.containing im)) [sS..sE]


_intersection_non_empty :: IM.IntervalMap Int [GffCount] -> (Int, Int) -> IM.IntervalMap Int [GffCount]
_intersection_non_empty im (sS, sE) = intersection' . filter (not . IM.null) $ im'
    where 
        im' = map (IM.fromList . (IM.containing im)) [sS..sE]


intersection' :: [IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]
intersection' [] = IM.empty
intersection' im = foldl (IM.intersection) (head im) im

--------------------

_allSameId :: IM.IntervalMap Int [GffCount] -> Bool
_allSameId im = all ((== sId) . annotSeqId) elems
    where
        sId   = annotSeqId . head $ elems
        elems = concat . IM.elems $ im

--------------------

intervals :: [GffLine] -> AnnotationMap
intervals = foldl (insertg) M.empty
    where
        insertg im g = M.alter (\mF -> updateF g mF) (gffType g) im
        updateF g mF = case mF of
            Nothing  -> Just $ updateF' g M.empty
            Just mF' -> Just $ updateF' g mF'

        updateF' g mF = M.alter (\v -> updateChrMap g v) (gffSeqId g) mF
        updateChrMap g v  = case v of
            Nothing -> Just $ insertCount g IM.empty
            Just a  -> Just $ insertCount g a


insertCount :: GffLine -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
insertCount g im = IM.alter (insertCount') intv im
    where 
          insertCount' Nothing  = Just [count]
          insertCount' (Just v) = Just $ count : v
          count = GffCount (gffId g) (gffType g) 0 (gffStrand g)  
          intv  = asInterval g


asInterval :: GffLine -> IM.Interval Int
asInterval g = IM.ClosedInterval (gffStart g) (gffEnd g)


_filterFeatures :: Maybe [String] -> GffLine -> Bool
_filterFeatures Nothing gf = (gffType gf) == GffGene
_filterFeatures (Just fs) gf = any matchFeature fs
    where
        g = gffType gf
        matchFeature "gene" = g == GffGene
        matchFeature "exon" = g == GffExon
        matchFeature "cds"  = g == GffCDS
        matchFeature "CDS"  = g == GffCDS
        matchFeature s = (show g) == s
