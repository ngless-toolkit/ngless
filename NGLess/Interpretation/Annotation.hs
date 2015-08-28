{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Interpretation.Annotation
    ( AnnotationIntersectionMode(..)
    , AnnotationOpts(..)
    , executeAnnotation
    , _annotate
    , _annotationRule
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

import ReferenceDatabases
import Output

import Data.GFF
import Data.Sam (SamLine(..), isAligned, isPositive, readAlignments)
import Data.AnnotRes
import Language
import NGLess
import Utils.Utils

type GffIMMap = IM.IntervalMap Int [GffCount]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> (Int, Int) -> GffIMMap

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

data AnnotationOpts =
    AnnotationOpts
    { optFeatures :: [GffType] -- ^ list of features to condider
    , optIntersectMode :: AnnotationRule
    , optStrandSpecific :: !Bool
    , optKeepAmbiguous :: !Bool
    }


executeAnnotation :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeAnnotation (NGOList e) args = NGOList <$> mapM (`executeAnnotation` args) e
executeAnnotation (NGOMappedReadSet e dDS) args = do
    ambiguity <- getBoolArg False "ambiguity" args
    strand_specific <- getBoolArg False "strand" args
    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (symbolOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur ("executeAnnotation: TYPE ERROR" :: String)
    m <- _annotationRule <$> parseAnnotationMode args
    g <- evalMaybeString $ lookup "gff" args
    gff <- whichAnnotationFile g dDS
    NGOAnnotatedSet <$> _annotate e gff AnnotationOpts
                { optFeatures = map matchingFeature fs
                , optIntersectMode = m
                , optStrandSpecific = strand_specific
                , optKeepAmbiguous = ambiguity
                }
executeAnnotation e _ = throwShouldNotOccur ("Annotation can handle MappedReadSet(s) only. Got " ++ show e)

parseAnnotationMode :: KwArgsValues -> NGLessIO AnnotationIntersectionMode
parseAnnotationMode args = case lookupWithDefault (NGOSymbol "union") "mode" args of
    (NGOSymbol "union") -> return  IntersectUnion
    (NGOSymbol "intersection_strict") -> return IntersectUnion
    (NGOSymbol "intersection_non_empty") -> return  IntersectNonEmpty
    m -> throwScriptError (concat ["Unexpected annotation mode (", show m, ")."])

whichAnnotationFile :: Maybe FilePath -- ^ explicitly passed GFF file
                -> Maybe T.Text -- ^ Reference
                -> NGLessIO FilePath -- ^ GFF to use
whichAnnotationFile (Just g) _ = do
    outputListLno' InfoOutput ["Annotate with given GFF: ", g]
    return g
whichAnnotationFile Nothing (Just dDs) = do
    outputListLno' InfoOutput ["Annotate with default GFF: ", show dDs]
    basedir <- ensureDataPresent (T.unpack dDs)
    return (getGff basedir) -- use default GFF
whichAnnotationFile Nothing Nothing =
    throwShouldNotOccur ("A gff must be provided by using the argument 'gff'" :: T.Text) -- not default ds and no gff passed as arg

_annotationRule :: AnnotationIntersectionMode -> AnnotationRule
_annotationRule IntersectUnion = union
_annotationRule IntersectStrict = _intersection_strict
_annotationRule IntersectNonEmpty = _intersection_non_empty


_annotate :: FilePath -> FilePath -> AnnotationOpts -> NGLessIO FilePath
_annotate samFp gffFp opts = do
        gffC <- liftIO $ intervals . filterFeats . readAnnotations <$> readPossiblyCompressedFile gffFp
        samC <- liftIO $ filter isAligned . readAlignments <$> readPossiblyCompressedFile samFp
        let res = calculateAnnotation gffC samC
        writeAnnotCount samFp (toGffM res)
    where
        filterFeats = filter (_matchFeatures $ optFeatures opts)
        calculateAnnotation :: AnnotationMap -> [SamLine] -> AnnotationMap
        calculateAnnotation aMap sam = aMap `deepseq` compStatsAnnot aMap sam opts

toGffM :: AnnotationMap -> [GffCount]
toGffM = concat . concat . map IM.elems . concat . map (M.elems) . M.elems

compStatsAnnot ::  AnnotationMap
                    -> [SamLine] -- ^ input data
                    -> AnnotationOpts
                    -> AnnotationMap
compStatsAnnot amap sam opts = foldl iterSam amap sam
    where
        iterSam am samline = M.map (M.alter alterCounts (samRName samline)) am
            where
                alterCounts Nothing = Nothing
                alterCounts (Just v) = Just $ annotateLine v samline opts

annotateLine :: GffIMMap -> SamLine -> AnnotationOpts -> GffIMMap
annotateLine im samline opts = uCounts matching im
    where
        sStart = samPos samline
        sEnd   = sStart + (samCigLen samline) - 1
        asStrand = if isPositive samline then GffPosStrand else GffNegStrand
        matching = maybeFilterAmbiguous (optKeepAmbiguous opts) .
                        maybeFilterStrand (optStrandSpecific opts) asStrand $ (optIntersectMode opts) im (sStart, sEnd)

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
    where intv = IM.ClosedInterval sS sE

_intersection_strict :: GffIMMap -> (Int, Int) -> GffIMMap
_intersection_strict im (sS, sE) = intersection' im'
    where im' = map (IM.fromList . (IM.containing im)) [sS..sE]


_intersection_non_empty :: GffIMMap -> (Int, Int) -> GffIMMap
_intersection_non_empty im (sS, sE) = intersection' . filter (not . IMG.null) $ im'
    where im' = map (IM.fromList . (IM.containing im)) [sS..sE]


intersection' :: [GffIMMap] -> GffIMMap
intersection' [] = IM.empty
intersection' im = foldl (IM.intersection) (head im) im

_allSameId :: GffIMMap -> Bool
_allSameId im = all ((== sId) . annotSeqId) elems
    where
        sId   = annotSeqId . head $ elems
        elems = concat . IM.elems $ im

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

matchingFeature :: T.Text -> GffType
matchingFeature "gene" = GffGene
matchingFeature "exon" = GffExon
matchingFeature "cds"  = GffCDS
matchingFeature "CDS"  = GffCDS
matchingFeature s      = GffOther (B8.pack . T.unpack $ s)

_matchFeatures :: [GffType] -> GffLine -> Bool
_matchFeatures fs gf = gffType gf `elem` fs

getBoolArg :: Bool -> T.Text -> KwArgsValues -> NGLessIO Bool
getBoolArg def k args = boolOrTypeError ("Argument '"++T.unpack k++"' for function 'annotate'") $ lookupWithDefault (NGOBool def) k args

evalMaybeString Nothing = return Nothing
evalMaybeString (Just (NGOString s)) = return (Just $ T.unpack s)
evalMaybeString o = throwShouldNotOccur ("evalString: Argument type must be NGOString (received " ++ show o ++ ").")


