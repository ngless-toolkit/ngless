{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}

module Interpretation.Annotation
    ( AnnotationIntersectionMode(..)
    , AnnotationOpts(..)
    , executeAnnotation
    , _annotate
    , _annotationRule
    , _intersection_strict
    , _matchFeatures
    , _allSameId
    ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import System.IO
import ReferenceDatabases
import Output

import Data.GFF
import Data.Sam (SamLine(..), samLength, isAligned, isPositive, readSamLine)
import Language
import FileManagement
import NGLess
import Utils.Utils
import Data.Annotation

type AnnotationInfo = (GffStrand, B.ByteString)
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [AnnotationInfo]

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
    ambiguity <- lookupBoolOrScriptErrorDef (return False) "annotation function" "ambiguity" args
    strand_specific <- lookupBoolOrScriptErrorDef (return False) "annotation function" "strand" args
    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (symbolOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur ("executeAnnotation: TYPE ERROR" :: String)
    m <- _annotationRule <$> parseAnnotationMode args
    g <- evalMaybeString $ lookup "gff" args
    gff <- whichAnnotationFile g dDS
    uncurry NGOAnnotatedSet <$> _annotate e gff AnnotationOpts
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
    (NGOSymbol "intersection_non_empty") -> return IntersectNonEmpty
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
    return (buildGFFPath basedir) -- use default GFF
whichAnnotationFile Nothing Nothing =
    throwShouldNotOccur ("A gff must be provided by using the argument 'gff'" :: T.Text) -- not default ds and no gff passed as arg

_annotationRule :: AnnotationIntersectionMode -> AnnotationRule
_annotationRule IntersectUnion = union
_annotationRule IntersectStrict = _intersection_strict
_annotationRule IntersectNonEmpty = _intersection_non_empty

_annotate :: FilePath -> FilePath -> AnnotationOpts -> NGLessIO (FilePath, FilePath)
_annotate samFp gffFp opts = do
        (newfp, h) <- openNGLTempFile samFp "annotated." "tsv"
        (newfp_headers, h_headers) <- openNGLTempFile samFp "annotation_headers." "txt"
        liftIO $ do
            amap <- intervals .  filterFeats .  readAnnotations <$> readPossiblyCompressedFile gffFp
            samC <- filter isAligned . readAlignments <$> readPossiblyCompressedFile samFp
            BL.hPut h . BL.concat $ concatMap (map encodeAR . annotateSamLine opts amap) samC
            hClose h

            BL.hPut h_headers . BL.fromChunks $ asHeaders amap
            hClose h_headers

            return (newfp, newfp_headers)
    where
        filterFeats = filter (_matchFeatures $ optFeatures opts)

readAlignments :: BL.ByteString -> [SamLine]
readAlignments = filter isSL . map readSamLine' . BL8.lines
    where
        isSL (SamHeader _) = False
        isSL SamLine{} = True
        readSamLine' :: BL.ByteString -> SamLine
        readSamLine' = rightOrError . readSamLine
        rightOrError (Right v) = v
        rightOrError (Left err) = error (show err)


asHeaders :: AnnotationMap -> [B.ByteString]
asHeaders amap = S.toList . S.fromList $ concatMap asHeaders' (M.assocs amap)
    where

        asHeaders' :: (GffType, M.Map B8.ByteString GffIMMap) -> [B.ByteString]
        asHeaders' (k,innermap) = concatMap (asHeaders'' k . snd) (M.assocs innermap)

        asHeaders'' :: GffType -> GffIMMap -> [B.ByteString]
        asHeaders'' k im = concatMap (asHeaders''' k . snd) (IM.toAscList im)

        asHeaders''' :: GffType -> [AnnotationInfo] -> [B.ByteString]
        asHeaders''' k vs = [B.concat [B8.pack . show $ k, "\t", snd v, "\n"] | v <- vs]

annotateSamLine :: AnnotationOpts -> AnnotationMap -> SamLine -> [AnnotatedRead]
annotateSamLine opts amap samline = concatMap annotateSamLine' (M.assocs amap)
    where
        rname = samRName samline
        sStart = samPos samline
        sEnd   = sStart + samLength samline - 1
        asStrand :: GffStrand
        asStrand = if optStrandSpecific opts
                        then if isPositive samline then GffPosStrand else GffNegStrand
                        else GffUnStranded
        annotateSamLine' (gtype,innermap) = case M.lookup rname innermap of
            Nothing -> []
            Just im -> map (buildAR gtype) . maybeFilterAmbiguous (optKeepAmbiguous opts)
                        $ (optIntersectMode opts) im asStrand (sStart, sEnd)
        buildAR gtype (_,name) = AnnotatedRead (samQName samline) name gtype asStrand


matchStrand :: GffStrand -> GffStrand -> Bool
matchStrand GffUnStranded _ = True
matchStrand _ GffUnStranded = True
matchStrand a b = a == b

maybeFilterAmbiguous  :: Bool -> [AnnotationInfo] -> [AnnotationInfo]
maybeFilterAmbiguous _ [] = []
maybeFilterAmbiguous True toU = toU
maybeFilterAmbiguous False ms
    | _allSameId ms = [head ms]
    | otherwise = [] -- ambiguous: discard

filterStrand :: GffStrand -> [(IM.Interval Int, [AnnotationInfo])] -> [(IM.Interval Int, [AnnotationInfo])]
filterStrand strand im = filter (not . null . snd) $ map (\(k,vs) ->
    (k, filter (matchStrand strand . fst) vs)) im

union :: AnnotationRule
union im strand (sS, sE) =  concatMap snd . (filterStrand strand) $ IM.intersecting im intv
    where intv = IM.ClosedInterval sS sE

_intersection_strict :: AnnotationRule
_intersection_strict im strand (sS, sE) = intersection' im'
    where im' = map (IM.fromList . filterStrand strand . (IM.containing im)) [sS..sE]

_intersection_non_empty :: AnnotationRule
_intersection_non_empty im strand (sS, sE) = intersection' im'
    where
        im' = map IM.fromList . filter (not . null) .  map (filterStrand strand . IM.containing subim) $ [sS..sE]
        subim = IM.fromList $ IM.intersecting im intv
        intv = IM.ClosedInterval sS sE

intersection' :: [GffIMMap] -> [AnnotationInfo]
intersection' [] = []
intersection' im = concat . IM.elems $ foldl1 IM.intersection im

_allSameId :: [AnnotationInfo] -> Bool
_allSameId = allSame . map snd

intervals :: [GffLine] -> AnnotationMap
intervals = foldl insertg M.empty
    where
        insertg am g = M.alter (updateF g) (gffType g) am
        updateF g mF = Just $ M.alter (updateChrMap g) (gffSeqId g) (fromMaybe M.empty mF)
        updateChrMap g v  = Just $ insertGffLine g (fromMaybe IM.empty v)

insertGffLine :: GffLine -> GffIMMap -> GffIMMap
insertGffLine g = IM.alter insertGffLine' asInterval
    where
        insertGffLine' Nothing  = Just [annot]
        insertGffLine' (Just vs) = Just (annot:vs)
        annot = (gffStrand g, gffId g)
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

evalMaybeString Nothing = return Nothing
evalMaybeString (Just (NGOString s)) = return (Just $ T.unpack s)
evalMaybeString o = throwShouldNotOccur ("evalString: Argument type must be NGOString (received " ++ show o ++ ").")


