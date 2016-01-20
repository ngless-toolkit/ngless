{- Copyright 2013-2016 NGLess Authors
 - License: MIT -}
{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts, TupleSections #-}

module Interpretation.Annotation
    ( AnnotationIntersectionMode(..)
    , AnnotationOpts(..)
    , executeAnnotation
    , _annotate
    , annotateMap
    , _annotateSeqname
    , _annotationRule
    , _intersection_strict
    , loadFunctionalMap
    , _matchFeatures
    ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import           Data.Conduit (($$), ($=), (=$), (=$=), ($$+), ($$+-))
import           Data.Conduit.Async (buffer, (=$=&), ($$&))

import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import GHC.Conc                 (getNumCapabilities)
import Data.List                (foldl1')
import Data.Monoid
import Data.Maybe
import System.IO

import ReferenceDatabases
import Output

import Data.GFF
import Data.Sam (SamLine(..), samLength, isAligned, isPositive, readSamGroupsC)
import Language
import FileManagement (openNGLTempFile)
import NGLess
import Utils.Conduit
import Utils.Utils
import Data.Annotation

type AnnotationInfo = (GffStrand, B.ByteString)
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [AnnotationInfo]

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

type GeneMapAnnotation = M.Map B8.ByteString [B8.ByteString]

loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO GeneMapAnnotation
loadFunctionalMap fname columns = do
        outputListLno' InfoOutput ["Loading map file ", fname]
        (resume, [headers]) <- (CB.sourceFile fname =$ CB.lines)
                $$+ (CL.isolate 1 =$= CL.map (B8.split '\t') =$ CL.consume)
        cis <- lookUpColumns headers
        gmap <- resume $$+-
                (CL.mapM (selectColumns cis . (B8.split '\t'))
                =$ CL.fold (flip $ uncurry M.insert) M.empty)
        outputListLno' TraceOutput ["Loading of map file '", fname, "' complete"]
        return gmap
    where
        lookUpColumns :: [B.ByteString] -> NGLessIO [Int]
        lookUpColumns [] = throwDataError ("Loading functional map file '" ++ fname ++ "': Header line missing!")
        lookUpColumns (_:headers) = do
            cis <- lookUpColumns' (zip [0..] headers)
            when (length cis /= length columns) $
                -- TODO: It would be best to have a more comprehensive error message (could not find header XYZ
                throwDataError ("Loading functional map file '" ++ fname ++ "': could not find all header columns")
            return cis
        lookUpColumns' [] = return []
        lookUpColumns' ((ix,v):vs)
            | v `elem` columns = do
                rest <- lookUpColumns' vs
                return (ix:rest)
            | otherwise = lookUpColumns' vs

        selectColumns :: [Int] -> [B.ByteString] -> NGLessIO (B.ByteString, [B.ByteString])
        selectColumns cols (gene:mapped) = (gene,) <$> selectIds cols (zip [0..] mapped)
        selectColumns _ [] = throwDataError ("Loading functional map file '" ++ fname ++ "': empty line.")

        selectIds :: [Int] -> [(Int, B.ByteString)] -> NGLessIO [B.ByteString]
        selectIds [] _ = return []
        selectIds fs@(fi:rest) ((ci,v):vs)
            | fi == ci = (v:) <$> selectIds rest vs
            | otherwise = selectIds fs vs
        selectIds _ _ = throwDataError ("Loading functional map file '" ++ fname ++ "': wrong number of columns")


data AnnotationOpts =
    AnnotationOpts
    { optFeatures :: [GffType] -- ^ list of features to condider
    , optIntersectMode :: AnnotationRule
    , optStrandSpecific :: !Bool
    , optKeepAmbiguous :: !Bool
    }


executeAnnotation :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeAnnotation (NGOList e) args = NGOList <$> mapM (`executeAnnotation` args) e
executeAnnotation (NGOMappedReadSet name samFp dDS) args = do
    ambiguity <- lookupBoolOrScriptErrorDef (return False) "annotation function" "ambiguity" args
    strand_specific <- lookupBoolOrScriptErrorDef (return False) "annotation function" "strand" args
    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (symbolOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur ("executeAnnotation: TYPE ERROR" :: String)
    m <- _annotationRule <$> parseAnnotationMode args
    let opts = AnnotationOpts
            { optFeatures = map matchingFeature fs
            , optIntersectMode = m
            , optStrandSpecific = strand_specific
            , optKeepAmbiguous = ambiguity
            }
    uncurry (NGOAnnotatedSet name) <$>
        if fs == ["seqname"]
            then _annotateSeqname samFp opts
            else do
                mapfile <- maybeFilePathOrTypeError $ lookup "mapfile" args
                case mapfile of
                    Nothing -> do
                        g <- maybeFilePathOrTypeError $ lookup "gff" args
                        gffFp <- whichAnnotationFile g dDS
                        _annotate samFp gffFp opts
                    Just mapfile' -> annotateMap samFp mapfile' opts
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


_annotateSeqname :: FilePath -> AnnotationOpts -> NGLessIO (FilePath, FilePath)
_annotateSeqname samFp opts = do
        outputListLno' InfoOutput ["Starting seqname annotation"]
        genericAnnotate seqName1 samFp Nothing
    where
        seqName1 :: [SamLine] -> [AnnotatedRead]
        seqName1 = mapMaybe seqAsAR
        seqAsAR sr@SamLine{samQName = rid, samRName = rname }
            | isAligned sr = Just (AnnotatedRead rid rname (GffOther "seqname") GffUnStranded)
        seqAsAR  _ = Nothing

flattenVmap :: (a -> [b]) -> V.Vector a -> V.Vector b
flattenVmap f vs = V.unfoldr access (0,[])
    where
        access (vi,[])
            | vi >= V.length vs = Nothing
            | otherwise = access (vi+1, f $ V.unsafeIndex vs vi)
        access (vi, x:xs) = Just (x, (vi,xs))



genericAnnotate :: ([SamLine] -> [AnnotatedRead]) -> FilePath -> Maybe [B.ByteString] -> NGLessIO (FilePath, FilePath)
genericAnnotate annot_function samfile headers = do
        (newfp, h) <- openNGLTempFile samfile "annotated." "tsv"
        (newfp_headers, h_headers) <- openNGLTempFile samfile "annotation_headers." "txt"
        nthreads <- liftIO getNumCapabilities
        ((), usednames) <-
            (conduitPossiblyCompressedFile samfile
                =$= CB.lines
                =$= readSamGroupsC
                =$= C.conduitVector 256)
                $$& (asyncMapC nthreads (flattenVmap annot_function))
                $=  CI.zipSinks
                    (asyncMapC nthreads (V.map encodeAR)
                        $= (C.awaitForever $ \vs ->
                            forM_ vs $ \v ->
                                liftIO (B.hPut h v)))
                    (case headers of
                        Just _ -> return S.empty
                        Nothing ->
                            asyncMapC nthreads (S.fromList . V.toList . V.map annotValue)
                            -- The lines below could be factored to an asyncFold :: (Monoid a, MonadIO m) -> Int -> Int -> C.Consumer a m a
                                $= C.conduitVector 12
                                $= asyncMapC nthreads (V.foldl (<>) mempty)
                                $= CL.fold S.union S.empty)
        liftIO $ forM_ (S.toAscList usednames) $ \name -> do
                B8.hPutStr h_headers "seqname\t"
                B8.hPutStrLn h_headers name
        liftIO (hClose h >> hClose h_headers)
        outputListLno' TraceOutput ["Finished annotation of file '", samfile, "'"]
        return (newfp, newfp_headers)
    where
        inserth s ar = S.insert (annotValue ar) s

annotateMap :: FilePath -> FilePath -> AnnotationOpts -> NGLessIO (FilePath, FilePath)
annotateMap samfile mapfile opts = do
    amap <- loadFunctionalMap mapfile (map getFeatureName $ optFeatures opts)
    let mapAnnotation :: AnnotationOpts -> [SamLine] -> [AnnotatedRead]
        mapAnnotation opts = concat . mapMaybe (mapAnnotation1 opts)
        mapAnnotation1 :: AnnotationOpts -> SamLine -> Maybe [AnnotatedRead]
        mapAnnotation1 _ samline = M.lookup (samRName samline) amap >>= \vs ->
            return [(AnnotatedRead (samQName samline) rname (GffOther "mapped") GffUnStranded) | rname <- vs]
    genericAnnotate (mapAnnotation opts) samfile Nothing

getFeatureName (GffOther s) = s
getFeatureName _ = error "getFeatureName called for non-GffOther input"


_annotate :: FilePath -> FilePath -> AnnotationOpts -> NGLessIO (FilePath, FilePath)
_annotate samfile gffFp opts = do
    amap <- readGffFile gffFp opts
    genericAnnotate (concatMap (annotateSamLine opts amap)) samfile (Just $ asHeaders amap)

readGffFile :: FilePath -> AnnotationOpts -> NGLessIO AnnotationMap
readGffFile gffFp opts = do
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "'..."]
        amap <- (conduitPossiblyCompressedFile gffFp
                $= CB.lines) `buffer1000`
                (readAnnotationOrDie
                =$= CL.filter (_matchFeatures $ optFeatures opts)
                =$ CL.fold insertg M.empty)
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "' complete."]
        return amap
    where
        readAnnotationOrDie :: C.Conduit B.ByteString NGLessIO GffLine
        readAnnotationOrDie = C.awaitForever $ \line ->
            unless (B8.head line == '#') $
                case readGffLine line of
                    Right g -> C.yield g
                    Left err -> throwError err
        insertg am g = M.alter (updateF g) (gffType g) am
        updateF g mF = Just $ M.alter (updateChrMap g) (gffSeqId g) (fromMaybe M.empty mF)
        updateChrMap g v  = Just $ insertGffLine g (fromMaybe IM.empty v)

asHeaders :: AnnotationMap -> [B.ByteString]
asHeaders amap = concatMap asHeaders' (M.assocs amap)
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
    | allSame (snd <$> ms) = [head ms]
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
intersection' im = concat . IM.elems $ foldl1' IM.intersection im



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


maybeFilePathOrTypeError Nothing = return Nothing
maybeFilePathOrTypeError (Just (NGOString s)) = return (Just $ T.unpack s)
maybeFilePathOrTypeError o = throwScriptError ("GFF String: Argument type must be NGOString (received " ++ show o ++ ").")
buffer1000 = buffer 1000

