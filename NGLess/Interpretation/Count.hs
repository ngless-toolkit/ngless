{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections #-}
module Interpretation.Count
    ( executeCount
    , CountOpts(..)
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

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit (($=), (=$), (=$=), ($$+), ($$+-))
import           Data.Conduit.Async (buffer, (=$=&), ($$&))

import Control.Monad
import Control.Arrow            (first)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import Data.List                (foldl1', transpose)
import GHC.Conc                 (getNumCapabilities)
import Data.Maybe
import Data.Monoid

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

type AnnotationInfo = (GffStrand, B.ByteString)
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
-- AnnotationMap maps from `GffType` to `References` (e.g., chromosomes) to positions to (features/count)
type AnnotationMap = M.Map GffType (M.Map B8.ByteString GffIMMap)

type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [AnnotationInfo]

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

type GeneMapAnnotation = M.Map B8.ByteString [B8.ByteString]

type FeatureSizeMap = M.Map B.ByteString Double

data MMMethod = MMCountAll | MM1OverN | MMDist1
    deriving (Eq, Show)

data CountOpts =
    CountOpts
    { optFeatures :: [GffType] -- ^ list of features to condider
    , optIntersectMode :: AnnotationRule
    , optStrandSpecific :: !Bool
    , optKeepAmbiguous :: !Bool
    , optMinCount :: !Double
    , optMMMethod :: !MMMethod
    , optDelim :: !B.ByteString
    }

data AnnotationMode = AnnotateSeqName | AnnotateGFF FilePath | AnnotateFunctionalMap FilePath
    deriving (Eq, Show)

data Annotator = SeqNameAnnotator | GFFAnnotator AnnotationMap | GeneMapAnnotator GeneMapAnnotation
    deriving (Eq, Show)

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> [[AnnotatedRead]]
annotateReadGroup _ SeqNameAnnotator = (:[]) . mapMaybe seqAsAR
    where
        seqAsAR sr@SamLine{samQName = rid, samRName = rname }
            | isAligned sr = Just (AnnotatedRead rid rname GffUnStranded)
        seqAsAR  _ = Nothing
annotateReadGroup opts (GFFAnnotator amap) = map (annotateSamLine opts amap)
annotateReadGroup opts (GeneMapAnnotator amap) = transpose . mapAnnotation
    where
        mapAnnotation :: [SamLine] -> [[AnnotatedRead]]
        mapAnnotation =  mapMaybe (mapAnnotation1 opts)
        mapAnnotation1 :: CountOpts -> SamLine -> Maybe [AnnotatedRead]
        mapAnnotation1 _ samline = M.lookup (samRName samline) amap >>= \vs ->
            return [AnnotatedRead (samQName samline) rname GffUnStranded | rname <- vs]

fillIndex :: Annotator -> M.Map B.ByteString Int -> M.Map B.ByteString Int
fillIndex annotator index = case annotator of
    SeqNameAnnotator -> index
    GFFAnnotator amap -> listToMapIndex (asHeaders amap)
    GeneMapAnnotator amap -> listToMapIndex (unravel amap)
  where
    unravel :: GeneMapAnnotation -> [B.ByteString]
    unravel amap = concat (M.elems amap)
    listToMapIndex :: [B.ByteString] -> M.Map B.ByteString Int
    listToMapIndex = loop 0 M.empty
    loop !_ !m [] = m
    loop n m (h:hs)
        | M.member h m = loop n m hs
        | otherwise = loop (n+1) (M.insert h n m) hs


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
    mocatMap <- lookupFilePath "functional_map argument to count()" "functional_map" args
    gffFile <- lookupFilePath "gff_file argument to count()" "gff_file" args

    delim <- T.encodeUtf8 <$> lookupStringOrScriptErrorDef (return "\t") "count hidden argument (should always be valid)" "__delim" args

    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (stringOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur ("executeAnnotation: TYPE ERROR" :: String)

    m <- annotationRule <$> parseAnnotationMode args
    let opts = CountOpts
            { optFeatures = map matchingFeature fs
            , optIntersectMode = m
            , optStrandSpecific = strand_specific
            , optKeepAmbiguous = ambiguity
            , optMinCount = fromInteger minCount
            , optMMMethod = method
            , optDelim = delim
            }
    amode <- annotationMode (optFeatures opts) (T.unpack <$> refinfo) (T.unpack <$> mocatMap) (T.unpack <$> gffFile)
    annotator <- loadAnnotator amode opts
    NGOCounts <$> performCount samfp rname annotator opts
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


loadAnnotator :: AnnotationMode -> CountOpts -> NGLessIO Annotator
loadAnnotator AnnotateSeqName _ = return SeqNameAnnotator
loadAnnotator (AnnotateGFF gf) opts = GFFAnnotator <$> loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = GeneMapAnnotator <$> loadFunctionalMap mm (map getFeatureName $ optFeatures opts)

performCount1Pass :: VUM.IOVector Double -> MMMethod -> C.Sink [Int] NGLessIO ((),[[Int]])
performCount1Pass mcounts method = case method of
    MMCountAll -> (,[]) <$> CL.mapM_ (liftIO . incrementAll mcounts)
    MM1OverN -> (,[]) <$> CL.mapM_ (liftIO . increment1OverN mcounts)
    MMDist1 -> CI.zipSinks
        (CL.mapM_ $ \case
                    [v] -> liftIO $ unsafeIncrement mcounts v
                    _ -> return ())
        (CL.filter ((/= 1) . length) $= CL.consume)

-- | Equivalent to Python's enumerate
enumerate :: (Monad m) => C.Conduit a m (Int, a)
enumerate = loop 0
    where
        loop !n = C.await >>= \case
                Just v -> C.yield (n, v) >> loop (n+1)
                Nothing -> return ()

extractSeqnames mapthreads = do
    let seqNameSize :: (Int, B.ByteString) -> Either NGError (B.ByteString, (Int, Double))
        seqNameSize (n, h) = do
            let tokens = B8.split '\t' h
            case tokens of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return (B.copy (B.drop 3 seqname), (n, convert size))
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")
        buildMap :: V.Vector (Int, B.ByteString) -> Either NGError (M.Map B.ByteString (Int,Double))
        buildMap pairs = M.fromList <$> (mapM seqNameSize (V.toList pairs))
    CL.filter ("@SQ\tSN:" `B.isPrefixOf`)
        =$= enumerate
        =$= C.conduitVector 1024
        =$= asyncMapEitherC mapthreads buildMap
        =$= CL.fold (<>) mempty


indexRead :: M.Map B.ByteString Int -> [AnnotatedRead] -> Either NGError [Int]
indexRead index rs = listNub <$> mapM index1 rs
    where
        listNub :: (Ord a) => [a] -> [a]
        listNub = S.toList . S.fromList
        index1 :: AnnotatedRead -> Either NGError Int
        index1 a = case M.lookup (annotValue a) index of
            Nothing -> throwShouldNotOccur ("Internal index missing an entry for " ++ (B8.unpack $ annotValue a))
            Just v -> return v

performCount :: FilePath -> T.Text -> Annotator -> CountOpts -> NGLessIO FilePath
performCount samfp gname annotator opts = do
    outputListLno' TraceOutput ["Starting count..."]
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        isSeqName SeqNameAnnotator = True
        isSeqName _ = False
        method = optMMMethod opts
        delim = optDelim opts

    (samcontent, index') <-
        conduitPossiblyCompressedFile samfp
            =$= CB.lines
            $$+ C.takeWhile ((=='@') . B8.head)
            =$= if isSeqName annotator
                    then M.map fst <$> extractSeqnames mapthreads
                    else (CL.sinkNull >> return M.empty)

    let index = fillIndex annotator index'
        n_headers = M.size index

    outputListLno' TraceOutput ["Loaded headers (", show n_headers, " headers); starting parsing/distribution."]
    mcounts <- liftIO $ VUM.replicate n_headers (0.0 :: Double)
    ((), toDistribute) <-
            samcontent
                $$+- readSamGroupsC
                =$= C.conduitVector 1024
                =$= asyncMapC mapthreads (sequence . flattenVmap (map (indexRead index) . annotateReadGroup opts annotator))
                =$= (C.awaitForever $ \case
                            Left err -> throwError err
                            Right v -> C.yield v)
                =$= (C.concat :: C.Conduit (V.Vector [Int]) NGLessIO [Int])
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
    liftIO $ do
        mheaders <- VM.new n_headers
        forM_ (M.toList index) $ \(v,i) ->
            VM.write mheaders i v
        headers <- V.unsafeFreeze mheaders
        BL.hPut hout (BL.fromChunks [delim, T.encodeUtf8 gname, "\n"])
        forM_ [0..VU.length result - 1] $ \i -> do
            let hn = (V.!) headers i
            v <- VU.indexM result i
            when (v >= optMinCount opts) $
                BL.hPut hout (BL.fromChunks [hn, "\t", B8.pack . show $ v, "\n"])
        hClose hout
    return newfp


incrementAll :: VUM.IOVector Double -> [Int] -> IO ()
incrementAll counts vis = forM_ vis $ \vi -> unsafeIncrement counts vi

increment1OverN :: VUM.IOVector Double -> [Int] -> IO ()
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

flattenVmap :: (a -> [b]) -> V.Vector a -> V.Vector b
flattenVmap f vs = V.unfoldr access (0,[])
    where
        access (!vi, x:xs) = Just (x, (vi,xs))
        access (!vi,[])
            | vi >= V.length vs = Nothing
            | otherwise = access (vi+1, f $ V.unsafeIndex vs vi)

getFeatureName (GffOther s) = s
getFeatureName _ = error "getFeatureName called for non-GffOther input"

loadGFF :: FilePath -> CountOpts -> NGLessIO AnnotationMap
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
        asHeaders''' k vs = [B.concat [B8.pack . show $ k, "\t", snd v] | v <- vs]

annotateSamLine :: CountOpts -> AnnotationMap -> SamLine -> [AnnotatedRead]
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
        buildAR gtype (_,name) = AnnotatedRead (samQName samline) (B.concat [B8.pack (show gtype),"\t",name]) asStrand


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

lookupFilePath context name args = case lookup name args of
    Nothing -> return Nothing
    Just a -> Just <$> stringOrTypeError context a

