{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections #-}
module Interpretation.Count
    ( executeCount
    , AnnotationOpts(..)
    , AnnotationMode(..)
    , AnnotationIntersectionMode(..)
    , MMMethod(..)
    , loadAnnotator
    , loadFunctionalMap
    , matchFeatures
    , annotationRule
    , performCount
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import qualified Data.HashTable.IO as H

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit (($=), (=$), (=$=), ($$+), ($$+-))
import           Data.Conduit.Async (buffer, (=$=&), ($$&))

import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import Data.List                (foldl1', nub)
import Data.Maybe

import System.IO                (hClose)
import Data.Convertible         (convert)

import Data.GFF
import Data.Sam (SamLine(..), samLength, isAligned, isPositive, readSamGroupsC)
import FileManagement (openNGLTempFile)
import ReferenceDatabases
import Data.Annotation
import Language
import Output
import NGLess

import Utils.Conduit
import Utils.Utils
import Utils.Vector
import Utils.Debug

type AnnotationInfo = (GffStrand, B.ByteString)
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [AnnotationInfo]

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

type GeneMapAnnotation = M.Map B8.ByteString [B8.ByteString]

data MMMethod = MMCountAll | MM1OverN | MMDist1
    deriving (Eq, Show)

data AnnotationOpts =
    AnnotationOpts
    { optFeatures :: [GffType] -- ^ list of features to condider
    , optIntersectMode :: AnnotationRule
    , optStrandSpecific :: !Bool
    , optKeepAmbiguous :: !Bool
    }

data AnnotationMode = AnnotateSeqName | AnnotateGFF FilePath | AnnotateFunctionalMap FilePath
    deriving (Eq, Show)

data Annotator = SeqNameAnnotator | GFFAnnotator AnnotationMap | GeneMapAnnotator GeneMapAnnotation
    deriving (Eq, Show)

isSeqName SeqNameAnnotator = True
isSeqName _ = False

annotateReadGroup :: AnnotationOpts -> Annotator -> [SamLine] -> [AnnotatedRead]
annotateReadGroup _ SeqNameAnnotator = mapMaybe seqAsAR
    where
        seqAsAR sr@SamLine{samQName = rid, samRName = rname }
            | isAligned sr = Just (AnnotatedRead rid rname (GffOther "seqname") GffUnStranded)
        seqAsAR  _ = Nothing
annotateReadGroup opts (GFFAnnotator amap) = concatMap (annotateSamLine opts amap)
annotateReadGroup opts (GeneMapAnnotator amap) = mapAnnotation
    where
        mapAnnotation :: [SamLine] -> [AnnotatedRead]
        mapAnnotation = concat . mapMaybe (mapAnnotation1 opts)
        mapAnnotation1 :: AnnotationOpts -> SamLine -> Maybe [AnnotatedRead]
        mapAnnotation1 _ samline = M.lookup (samRName samline) amap >>= \vs ->
            return [AnnotatedRead (samQName samline) rname (GffOther "mapped") GffUnStranded | rname <- vs]

fillIndex SeqNameAnnotator _ = return ()
fillIndex (GFFAnnotator amap) h =
    forM_ (zip (asHeaders amap) [0..]) $ \(hd, i) ->
        liftIO (H.insert h hd i)
fillIndex (GeneMapAnnotator amap) h =
    forM_ (zip (unravel amap) [0..]) $ \(hd, i) ->
        liftIO (H.insert h hd i)

unravel :: GeneMapAnnotation -> [B.ByteString]
unravel amap = concat (M.elems amap)


methodFor "1overN" = return MM1OverN
methodFor "dist1" = return MMDist1
methodFor "all1" = return MMCountAll
methodFor other = throwShouldNotOccur (T.concat ["Unexpected multiple method ", other])

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOMappedReadSet rname samfp refinfo) args = do
    let c = lookup "counts" args
        c' = GffGene
    minCount <- lookupIntegerOrScriptErrorDef (return 0) "count argument parsing" "min" args
    method <- methodFor =<< lookupSymbolOrScriptErrorDef (return "dist1")
                                    "multiple argument to count " "multiple" args
    ambiguity <- lookupBoolOrScriptErrorDef (return False) "annotation function" "ambiguity" args
    strand_specific <- lookupBoolOrScriptErrorDef (return False) "annotation function" "strand" args
    mocatMap <- case lookup "functional_map" args of
                    Nothing -> return Nothing
                    Just a -> Just <$> stringOrTypeError "functional_map argument to count()" a
    gffFile <- case lookup "gff_file" args of
                    Nothing -> return Nothing
                    Just a -> Just <$> stringOrTypeError "gff_file argument to count()" a

    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (symbolOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur ("executeAnnotation: TYPE ERROR" :: String)

    m <- annotationRule <$> parseAnnotationMode args
    let opts = AnnotationOpts
            { optFeatures = map matchingFeature fs
            , optIntersectMode = m
            , optStrandSpecific = strand_specific
            , optKeepAmbiguous = ambiguity
            }
    amode <- annotationMode (optFeatures opts) (T.unpack <$> refinfo) (T.unpack <$> mocatMap) (T.unpack <$> gffFile)
    annotator <- loadAnnotator amode opts
    NGOCounts <$> performCount samfp rname annotator opts method minCount
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


loadAnnotator :: AnnotationMode -> AnnotationOpts -> NGLessIO Annotator
loadAnnotator AnnotateSeqName _ = return SeqNameAnnotator
loadAnnotator (AnnotateGFF gf) opts = GFFAnnotator <$> loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = GeneMapAnnotator <$> loadFunctionalMap mm (map getFeatureName $ optFeatures opts)

performCount1Pass :: VUM.IOVector Double -> MMMethod -> C.Sink [Int] NGLessIO ((),[[Int]])
performCount1Pass mcounts  method =
    CL.map nub
        $= case method of
            MMCountAll -> (,[]) <$> CL.mapM_ (liftIO . incrementAll mcounts)
            MM1OverN -> (,[]) <$> CL.mapM_ (liftIO . increment1OverN mcounts)
            MMDist1 -> CI.zipSinks
                (CL.mapM_ $ \case
                            [v] -> liftIO $ unsafeIncrement mcounts v
                            _ -> return ())
                (CL.filter ((/= 1) . length) $= CL.consume)
enumerate :: (Monad m) => C.Conduit a m (Int,a)
enumerate = loop 0
    where
        loop !n = C.await >>= \case
                Just v -> C.yield (n, v) >> loop (n+1)
                Nothing -> return ()

extractSeqnames h = do
    let seqName :: B.ByteString -> B.ByteString
        seqName = B.copy . B.drop 1 .  (!! 0) . B8.split ' '
        inserth (i,n) = liftIO $ H.insert h n (i :: Int)
    CL.map seqName
        =$= enumerate
        =$= CL.mapM_ inserth

hashSize = H.foldM (\p _ -> return (p+1)) 0

indexRead h ar =
    forM ar $ \a -> liftIO (H.lookup h $ annotValue a) >>= \case
            Nothing -> throwShouldNotOccur ("Internal index missing an entry" :: String)
            Just v -> return v

performCount :: FilePath -> T.Text -> Annotator -> AnnotationOpts -> MMMethod -> Integer -> NGLessIO FilePath
performCount samfp gname annotator opts method minCount = do
    outputListLno' TraceOutput ["Starting count..."]
    h <- liftIO (H.new :: IO (H.BasicHashTable B.ByteString Int))
    (samcontent, ()) <-
        conduitPossiblyCompressedFile samfp
            =$= CB.lines
            $$+ C.takeWhile ((=='@') . B8.head)
            =$= if isSeqName annotator
                    then extractSeqnames h
                    else CL.sinkNull
    fillIndex annotator h
    n_headers <- liftIO $ hashSize h

    mcounts <- liftIO $ VUM.replicate n_headers (0.0 :: Double)
    ((), toDistribute) <-
            samcontent
                $$+- readSamGroupsC
                =$= CL.map (annotateReadGroup opts annotator)
                =$= CL.mapM (indexRead h)
                $= performCount1Pass mcounts method
    counts <- liftIO $ VU.unsafeFreeze mcounts

    result <-
        if method /= MMDist1
            then return counts
            else do
                outputListLno' TraceOutput ["Counts (second pass)..."]
                let secondpass = distributeMM toDistribute counts False
                return secondpass

    (newfp,hout) <- openNGLTempFile samfp "counts." "txt"
    headers <- liftIO $ extractHeaders n_headers h
    liftIO $ do
        BL.hPut hout (BL.fromChunks ["\t", T.encodeUtf8 gname, "\n"])
        forM_ [0..VU.length result - 1] $ \i -> do
            let hn = (V.!) headers i
            v <- VU.indexM result i
            when (v > fromIntegral minCount) $
                BL.hPut hout (BL.fromChunks [hn, "\t", B8.pack . show $ v, "\n"])
        hClose hout
    return newfp

extractHeaders n_headers h = do
    res <- VM.new n_headers
    flip H.mapM_ h $ \(v,i) ->
        VM.write res i v
    V.unsafeFreeze res

--incrementAll :: VUM.IOVector Double -> [Int] -> NGLessIO ()
incrementAll counts vis = forM_ vis $ \vi -> unsafeIncrement counts vi

--increment1OverN :: VUM.IOVector Double -> [Int] -> NGLessIO ()
increment1OverN counts vis = forM_ vis $ \vi -> unsafeIncrement' counts vi (1.0 / nc)
    where
        nc :: Double
        nc = 1.0 / convert (length vis)

distributeMM indices current fractionResult = VU.create $ do
    ncounts <- VU.thaw current -- note that thaw performs a copy
    forM_ indices $ \vs -> do
        let cs = map (VU.unsafeIndex current) vs
            cs_sum = sum cs
            n_cs = convert (length cs)
            adjust :: Double -> Double
            adjust = if cs_sum > 0.0
                        then (/ cs_sum)
                        else const  (1.0 / n_cs)
        forM_ (zip vs cs) $ \(v,c) ->
            unsafeIncrement' ncounts v (adjust c)
    when fractionResult $
        toFractions ncounts
    return ncounts


loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO GeneMapAnnotation
loadFunctionalMap fname columns = do
        outputListLno' InfoOutput ["Loading map file ", fname]
        (resume, [headers]) <- (CB.sourceFile fname =$ CB.lines)
                $$+ (CL.isolate 1 =$= CL.map (B8.split '\t') =$ CL.consume)
        cis <- lookUpColumns headers
        gmap <- resume $$+-
                (CL.mapM (selectColumns cis . B8.split '\t')
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

parseAnnotationMode :: KwArgsValues -> NGLessIO AnnotationIntersectionMode
parseAnnotationMode args = case lookupWithDefault (NGOSymbol "union") "mode" args of
    (NGOSymbol "union") -> return  IntersectUnion
    (NGOSymbol "intersection_strict") -> return IntersectUnion
    (NGOSymbol "intersection_non_empty") -> return IntersectNonEmpty
    m -> throwScriptError (concat ["Unexpected annotation mode (", show m, ")."])


annotationMode :: [GffType] -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> NGLessIO AnnotationMode
annotationMode _ _ (Just _) (Just _) = throwScriptError ("Cannot simmultaneously pass a gff_file and an annotation_file for annotate() function" :: String)
annotationMode [GffOther "seqname"] _ _ _ = return AnnotateSeqName
annotationMode _ _ (Just r) _ = return (AnnotateFunctionalMap r)
annotationMode _ _ _ (Just g) = return (AnnotateGFF g)
annotationMode _ (Just ref) Nothing Nothing = do
    outputListLno' InfoOutput ["Annotate with default GFF: ", show ref]
    basedir <- ensureDataPresent ref
    return (AnnotateGFF $ buildGFFPath basedir) -- use default GFF
annotationMode _ _ _ _ =
            throwShouldNotOccur ("For counting, you must do one of\n1. use seqname mode\n2. pass in a GFF file using the argument 'gff_file'\n3. pass in a gene map using the argument 'functional_map'" :: T.Text)

annotationRule :: AnnotationIntersectionMode -> AnnotationRule
annotationRule IntersectUnion = union
annotationRule IntersectStrict = intersection_strict
annotationRule IntersectNonEmpty = intersection_non_empty

flattenVmap :: (VUM.Unbox a, VUM.Unbox b) => (a -> [b]) -> VU.Vector a -> VU.Vector b
flattenVmap f vs = VU.unfoldr access (0,[])
    where
        access (!vi, x:xs) = Just (x, (vi,xs))
        access (!vi,[])
            | vi >= VU.length vs = Nothing
            | otherwise = access (vi+1, f $ VU.unsafeIndex vs vi)

getFeatureName (GffOther s) = s
getFeatureName _ = error "getFeatureName called for non-GffOther input"

loadGFF :: FilePath -> AnnotationOpts -> NGLessIO AnnotationMap
loadGFF gffFp opts = do
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "'..."]
        amap <- (conduitPossiblyCompressedFile gffFp
                $= CB.lines)
                $$& (readAnnotationOrDie
                =$= CL.filter (matchFeatures $ optFeatures opts)
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
            Nothing -> trace "GFF lookup empty" []
            Just im -> map (buildAR gtype) . maybeFilterAmbiguous (optKeepAmbiguous opts)
                        $ (optIntersectMode opts) im asStrand (sStart, sEnd)
        buildAR gtype (_,name) = trace "GFF found something" $ AnnotatedRead (samQName samline) name gtype asStrand


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

intersection_strict :: AnnotationRule
intersection_strict im strand (sS, sE) = intersection' im'
    where im' = map (IM.fromList . filterStrand strand . (IM.containing im)) [sS..sE]

intersection_non_empty :: AnnotationRule
intersection_non_empty im strand (sS, sE) = intersection' im'
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

matchFeatures :: [GffType] -> GffLine -> Bool
matchFeatures fs gf = gffType gf `elem` fs

maybeFilePathOrTypeError Nothing = return Nothing
maybeFilePathOrTypeError (Just (NGOString s)) = return (Just $ T.unpack s)
maybeFilePathOrTypeError o = throwScriptError ("GFF String: Argument type must be NGOString (received " ++ show o ++ ").")

