{-# LANGUAGE OverloadedStrings #-}

module Annotation
    ( AnnotationIntersectionMode(..)
    , annotate
    , _intersection_strict
    , _intersection_non_empty
    , _matchFeatures
    , _allSameId
    ) where


import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM
import qualified Data.IntervalMap.Generic.Strict as IMG
import qualified Data.Map.Strict as M

import Control.Applicative
import Control.DeepSeq

import FileManagement(readPossiblyCompressedFile)
import ReferenceDatabases
import Output

import Data.GFF
import Data.Sam (SamLine(..), isAligned, isPositive, readAlignments)
import Data.AnnotRes

type AnnotationMap = M.Map GffType (M.Map B8.ByteString (IM.IntervalMap Int [GffCount]))

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

-- |Annotates mapped reads according to a mapping
annotate :: FilePath                            -- ^ input SAM file
                -> Maybe FilePath               -- ^ input GFF file
                -> Maybe [String]               -- ^ list of features
                -> Maybe T.Text                 -- ^ reference information (if std ref was used)
                -> AnnotationIntersectionMode   -- ^ mode
                -> Bool                         -- ^ ambiguity
                -> Bool                         -- ^ stranded
                -> IO FilePath
annotate samFP (Just g) feats _ m a s = do
    outputListLno' InfoOutput ["Annotate with given GFF: ", g]
    annotate' samFP g feats (getIntervalQuery m) a s
annotate samFP Nothing feats (Just dDs) m a s = do
    outputListLno' InfoOutput ["Annotate with default GFF: ", show dDs]
    basedir <- ensureDataPresent (T.unpack dDs)
    annotate' samFP (getGff basedir) feats (getIntervalQuery m) a s   -- use default GFF
annotate _     Nothing _ Nothing _ _ _ =
    error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: AnnotationIntersectionMode -> (IM.IntervalMap Int [GffCount] -> (Int, Int) -> IM.IntervalMap Int [GffCount])
getIntervalQuery IntersectUnion = union
getIntervalQuery IntersectStrict = _intersection_strict
getIntervalQuery IntersectNonEmpty = _intersection_non_empty


annotate' :: FilePath -> FilePath -> Maybe [String] -> (IM.IntervalMap Int [GffCount] -> (Int,Int) -> IM.IntervalMap Int [GffCount]) -> Bool -> Bool -> IO FilePath
annotate' samFp gffFp feats a f s = do
        gffC <- intervals . filterFeats . readAnnotations <$> readPossiblyCompressedFile gffFp
        samC <- filter isAligned . readAlignments <$> readPossiblyCompressedFile samFp
        let res = calculateAnnotation gffC samC
        writeAnnotCount samFp (toGffM . concat . map (M.elems) . M.elems $ res)
    where
        filterFeats = filter (_matchFeatures $ matchingFeatures feats)
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
filterStrand True  s m =  IMG.filter (not . null) . IMG.map (filterByStrand s) $ m
filterStrand False _ m = m


countsAmbiguity :: Bool -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
countsAmbiguity True toU imR = uCounts toU imR
countsAmbiguity False toU imR
    | IMG.null toU = imR -- no_feature
    | not (_allSameId toU) = imR -- ambiguous
    | otherwise = uCounts (IM.fromList . take 1 . IM.toList $ toU) imR -- same feature multiple times. increase that feature ONCE.


uCounts :: IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
uCounts keys im = IM.foldlWithKey (\res k _ -> IM.adjust incCount k res) im keys
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
_intersection_non_empty im (sS, sE) = intersection' . filter (not . IMG.null) $ im'
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
intervals = foldl insertg M.empty
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
insertCount g im = IM.alter insertCount' asInterval im
    where
        insertCount' Nothing  = Just [count]
        insertCount' (Just v) = Just (count:v)
        count = GffCount (gffId g) (gffType g) 0 (gffStrand g)
        asInterval :: IM.Interval Int
        asInterval = IM.ClosedInterval (gffStart g) (gffEnd g)

matchingFeatures :: Maybe [String] -> [GffType]
matchingFeatures Nothing = [GffGene]
matchingFeatures (Just fs) = map toFeature fs
    where
        toFeature "gene" = GffGene
        toFeature "exon" = GffExon
        toFeature "cds"  = GffCDS
        toFeature "CDS"  = GffCDS
        toFeature s      = GffOther (B8.pack s)

_matchFeatures :: [GffType] -> GffLine -> Bool
_matchFeatures fs gf = any (== gffType gf) fs
