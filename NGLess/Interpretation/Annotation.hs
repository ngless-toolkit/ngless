{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Annotation
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
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

import Utils.Utils (readPossiblyCompressedFile)
import ReferenceDatabases
import Output

import Data.GFF
import Data.Sam (SamLine(..), isAligned, isPositive, readAlignments)
import Data.AnnotRes
import NGLess

type GffIMMap = IM.IntervalMap Int [GffCount]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> (Int, Int) -> GffIMMap

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
                -> NGLessIO FilePath
annotate samFP (Just g) feats _ m a s = do
    outputListLno' InfoOutput ["Annotate with given GFF: ", g]
    annotate' samFP g feats (getIntervalQuery m) a s
annotate samFP Nothing feats (Just dDs) m a s = do
    outputListLno' InfoOutput ["Annotate with default GFF: ", show dDs]
    basedir <- ensureDataPresent (T.unpack dDs)
    annotate' samFP (getGff basedir) feats (getIntervalQuery m) a s   -- use default GFF
annotate _     Nothing _ Nothing _ _ _ =
    error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: AnnotationIntersectionMode -> AnnotationRule
getIntervalQuery IntersectUnion = union
getIntervalQuery IntersectStrict = _intersection_strict
getIntervalQuery IntersectNonEmpty = _intersection_non_empty


annotate' :: FilePath
                -> FilePath
                -> Maybe [String]
                -> AnnotationRule
                -> Bool
                -> Bool
                -> NGLessIO FilePath
annotate' samFp gffFp feats f a s = do
        gffC <- liftIO $ intervals . filterFeats . readAnnotations <$> readPossiblyCompressedFile gffFp
        samC <- liftIO $ filter isAligned . readAlignments <$> readPossiblyCompressedFile samFp
        let res = calculateAnnotation gffC samC
        writeAnnotCount samFp (toGffM res)
    where
        filterFeats = filter (_matchFeatures $ matchingFeatures feats)
        calculateAnnotation :: AnnotationMap -> [SamLine] -> AnnotationMap
        calculateAnnotation aMap sam = aMap `deepseq` compStatsAnnot aMap sam f a s

toGffM :: AnnotationMap -> [GffCount]
toGffM = concat . concat . map IM.elems . concat . map (M.elems) . M.elems

compStatsAnnot ::  AnnotationMap
                    -> [SamLine] -- ^ input data
                    -> AnnotationRule -- ^ annotation rule
                    -> Bool      -- ^ whether ambiguous matches count
                    -> Bool      -- ^ whether strand must match
                    -> AnnotationMap
compStatsAnnot amap sam f a s = foldl iterSam amap sam
    where
        iterSam am y = M.map (M.alter alterCounts (samRName y)) am
            where
                alterCounts Nothing = Nothing
                alterCounts (Just v) = Just $ annotateLine f v y a s


annotateLine :: AnnotationRule -> GffIMMap -> SamLine -> Bool -> Bool -> GffIMMap
annotateLine f im y a s = uCounts matching im
    where
        sStart = samPos y
        sEnd   = sStart + (samCigLen y) - 1
        asStrand = if isPositive y then GffPosStrand else GffNegStrand
        matching = maybeFilterAmbiguous a . maybeFilterStrand s asStrand $ f im (sStart, sEnd)

maybeFilterStrand :: Bool -> GffStrand -> GffIMMap -> GffIMMap
maybeFilterStrand True  s =  IMG.filter (not . null) . IMG.map (filterByStrand s)
maybeFilterStrand False _ = id


maybeFilterAmbiguous  :: Bool -> GffIMMap -> GffIMMap
maybeFilterAmbiguous True toU = toU
maybeFilterAmbiguous False ms
    | IMG.null ms = IM.empty
    | _allSameId ms = (IM.fromList . take 1 . IM.toList) ms -- same feature multiple times, count just once
    | otherwise = IM.empty -- ambiguous: discard


uCounts :: GffIMMap -> GffIMMap -> GffIMMap
uCounts keys im = IM.foldlWithKey (\res k _ -> IM.adjust incCount k res) im keys
    where
        incCount []     = []
        incCount (x:rs) = incCount' x : rs
        incCount' (GffCount gId gT !gC gS) = (GffCount gId gT (gC + 1) gS)

--- Diferent modes

union :: GffIMMap -> (Int, Int) -> GffIMMap
union im (sS, sE) = IM.fromList $ IM.intersecting im intv
    where
        intv = IM.ClosedInterval sS sE

_intersection_strict :: GffIMMap -> (Int, Int) -> GffIMMap
_intersection_strict im (sS, sE) = intersection' im'
    where 
        im' = map (IM.fromList . (IM.containing im)) [sS..sE]


_intersection_non_empty :: GffIMMap -> (Int, Int) -> GffIMMap
_intersection_non_empty im (sS, sE) = intersection' . filter (not . IMG.null) $ im'
    where 
        im' = map (IM.fromList . (IM.containing im)) [sS..sE]


intersection' :: [GffIMMap] -> GffIMMap
intersection' [] = IM.empty
intersection' im = foldl (IM.intersection) (head im) im

--------------------

_allSameId :: GffIMMap -> Bool
_allSameId im = all ((== sId) . annotSeqId) elems
    where
        sId   = annotSeqId . head $ elems
        elems = concat . IM.elems $ im

--------------------

intervals :: [GffLine] -> AnnotationMap
intervals = foldl insertg M.empty
    where
        insertg am g = M.alter (updateF g) (gffType g) am
        updateF g mF = Just $ M.alter (updateChrMap g) (gffSeqId g) (fromMaybe M.empty mF)
        updateChrMap g v  = Just $ insertZeroCount g (fromMaybe IM.empty v)


insertZeroCount :: GffLine -> GffIMMap -> GffIMMap
insertZeroCount g im = IM.alter insertZeroCount' asInterval im
    where
        insertZeroCount' Nothing  = Just [count]
        insertZeroCount' (Just v) = Just (count:v)
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
