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
import Language
import Output
import NGLess

import Utils.Conduit
import Utils.Utils
import Utils.Vector


-- GFFAnnotationMap maps from `References` (e.g., chromosomes) to positions to (strand/feature-id)
type AnnotationInfo = (GffStrand, Int)
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
type GFFAnnotationMap = M.Map B.ByteString GffIMMap
type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [(GffStrand,Int)]

data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

type GeneMapAnnotation = M.Map B8.ByteString [Int]

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
    , optNormSize :: !Bool
    }

data AnnotationMode = AnnotateSeqName | AnnotateGFF FilePath | AnnotateFunctionalMap FilePath
    deriving (Eq, Show)

data Annotator =
                SeqNameAnnotator (Maybe (M.Map B.ByteString (Int, Double)))
                | GFFAnnotator GFFAnnotationMap [B.ByteString] FeatureSizeMap
                | GeneMapAnnotator GeneMapAnnotation [B.ByteString] FeatureSizeMap
    deriving (Eq, Show)

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> Either NGError [[Int]]
annotateReadGroup _ (SeqNameAnnotator Nothing) _ = throwShouldNotOccur ("Incomplete annotator used" :: String)
annotateReadGroup _ (SeqNameAnnotator (Just szmap)) samlines = (:[]) . listNub <$> mapMaybeM getID samlines
    where
        getID :: SamLine -> Either NGError (Maybe Int)
        getID sr@SamLine{samRName = rname }
            | isAligned sr = case M.lookup rname szmap of
                    Just (ix,_) -> Right (Just ix)
                    Nothing -> throwDataError ("Unknown sequence id: " ++ show rname)
        getID  _ = Right Nothing
annotateReadGroup opts (GFFAnnotator amap _ _) samlines = return . map (annotateSamLine opts amap) $ samlines
annotateReadGroup opts (GeneMapAnnotator amap _ _) samlines = return . transpose . mapAnnotation $ samlines
    where
        mapAnnotation :: [SamLine] -> [[Int]]
        mapAnnotation =  mapMaybe (mapAnnotation1 opts)
        mapAnnotation1 :: CountOpts -> SamLine -> Maybe [Int]
        mapAnnotation1 _ samline = M.lookup (samRName samline) amap

finishAnnotator :: Annotator -> M.Map B.ByteString (Int,Double) -> Annotator
finishAnnotator SeqNameAnnotator{} idszmap = SeqNameAnnotator (Just idszmap)
finishAnnotator ann _ = ann

annSizeOf :: Annotator -> B.ByteString -> Either NGError Double
annSizeOf (SeqNameAnnotator Nothing) _ = throwShouldNotOccur ("Using unloaded annotator" :: String)
annSizeOf (SeqNameAnnotator (Just ix)) name = snd <$> annSizeOf' name ix
annSizeOf (GFFAnnotator _ _ szmap) name = annSizeOf' name szmap
annSizeOf (GeneMapAnnotator _ _ szmap) name = annSizeOf' name szmap
annSizeOf' name ix = case M.lookup name ix of
    Just s -> return s
    Nothing -> throwShouldNotOccur ("Header does not exist in sizes: "++show name)


annEnumerate :: Annotator -> [(B.ByteString, Int)]
annEnumerate (SeqNameAnnotator Nothing) = error "Using unfinished annotator"
annEnumerate (SeqNameAnnotator (Just ix)) = M.assocs $ M.map fst ix
annEnumerate (GFFAnnotator _ headers _) = zip headers [0..]
annEnumerate (GeneMapAnnotator _ headers _) = zip headers [0..]

annSize :: Annotator -> Int
annSize ann = length (annEnumerate ann)

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
    normSize <- lookupBoolOrScriptErrorDef (return False) "count function" "norm" args

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
            , optNormSize = normSize
            }
    amode <- annotationMode (optFeatures opts) (T.unpack <$> refinfo) (T.unpack <$> mocatMap) (T.unpack <$> gffFile)
    annotator <- loadAnnotator amode opts
    NGOCounts <$> performCount samfp rname annotator opts
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


loadAnnotator :: AnnotationMode -> CountOpts -> NGLessIO Annotator
loadAnnotator AnnotateSeqName _ = return $ SeqNameAnnotator Nothing
loadAnnotator (AnnotateGFF gf) opts = loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = loadFunctionalMap mm (map getFeatureName $ optFeatures opts)

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

annSamHeaderParser :: Int -> Annotator -> C.Sink ByteLine NGLessIO (M.Map B.ByteString (Int, Double))
annSamHeaderParser mapthreads SeqNameAnnotator{} = do
    let seqNameSize :: (Int, ByteLine) -> Either NGError (B.ByteString, (Int, Double))
        seqNameSize (n, ByteLine h) = do
            let tokens = B8.split '\t' h
            case tokens of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return (B.copy (B.drop 3 seqname), (n, convert size))
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")
        buildMap :: V.Vector (Int, ByteLine) -> Either NGError (M.Map B.ByteString (Int,Double))
        buildMap pairs = M.fromList <$> mapM seqNameSize (V.toList pairs)
    CL.filter ((B.isPrefixOf "@SQ\tSN:") . unwrapByteLine)
        =$= enumerate
        =$= C.conduitVector 1024
        =$= asyncMapEitherC mapthreads buildMap
        =$= CL.fold (<>) mempty
annSamHeaderParser _ _ = C.sinkNull >> return M.empty


listNub :: (Ord a) => [a] -> [a]
listNub = S.toList . S.fromList

performCount :: FilePath -> T.Text -> Annotator -> CountOpts -> NGLessIO FilePath
performCount samfp gname annotator0 opts = do
    outputListLno' TraceOutput ["Starting count..."]
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        method = optMMMethod opts
        delim = optDelim opts

    (samcontent, headerinfo) <-
        conduitPossiblyCompressedFile samfp
            =$= linesC
            $$+ C.takeWhile ((=='@') . B8.head . unwrapByteLine)
            =$= annSamHeaderParser mapthreads annotator0
    let annotator = finishAnnotator annotator0 headerinfo
        n_entries = annSize annotator

    outputListLno' TraceOutput ["Loaded headers (", show n_entries, " headers); starting parsing/distribution."]
    mcounts <- liftIO $ VUM.replicate n_entries (0.0 :: Double)
    ((), toDistribute) <-
            samcontent
                $$+- readSamGroupsC
                =$= C.conduitVector 1024
                =$= asyncMapEitherC mapthreads (flattenEitherVmap (annotateReadGroup opts annotator))
                =$= (C.concat :: C.Conduit (V.Vector [Int]) NGLessIO [Int])
                $= performCount1Pass mcounts method

    when (optNormSize opts) $ do
        sizes <- liftIO $ VUM.new n_entries
        forM_ (annEnumerate annotator) $ \(name,i) -> do
            s <- runNGLess $ annSizeOf annotator name
            liftIO $ VUM.write sizes i s
        normalizeCounts mcounts sizes
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
        mheaders <- VM.new n_entries
        forM_ (annEnumerate annotator) $ \(v,i) ->
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

normalizeCounts :: VUM.IOVector Double -> VUM.IOVector Double -> NGLessIO ()
normalizeCounts counts sizes = do
    let n = VUM.length counts
        n' = VUM.length sizes
    unless (n == n') $
        throwShouldNotOccur ("Counts vector is of size " ++ show n ++ ", but sizes if of size " ++ show n')
    forM_ [0 .. n - 1] $ \i -> liftIO $ do
        s <- VUM.read sizes i
        unsafeModify counts (/ s) i

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


loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO Annotator
loadFunctionalMap fname columns = do
        outputListLno' InfoOutput ["Loading map file ", fname]
        (resume, [headers]) <- (CB.sourceFile fname =$ CB.lines)
                $$+ (CL.isolate 1 =$= CL.map (B8.split '\t') =$ CL.consume)
        cis <- lookUpColumns headers
        (_,gmap,namemap) <- resume $$+-
                (CL.mapM (selectColumns cis . B8.split '\t')
                =$ CL.fold inserts (0, M.empty,M.empty))
        outputListLno' TraceOutput ["Loading of map file '", fname, "' complete"]
        return $ GeneMapAnnotator gmap (M.keys namemap) M.empty
    where
        inserts :: (Int, M.Map B.ByteString [Int], M.Map B.ByteString Int) -> (B.ByteString, [B.ByteString]) -> (Int, M.Map B.ByteString [Int], M.Map B.ByteString Int)
        inserts (next, gmap, namemap) (name, ids) = (next', gmap', namemap')
            where
                (next', ids', namemap') = mapnames next ids namemap

                mapnames next [] curmap = (next, [], curmap)
                mapnames next (n:ns) curmap = case M.lookup n curmap of
                    Nothing -> let  nextmap = M.insert n next curmap
                                    (next', ns', finalmap) = mapnames (next+1) ns nextmap
                                in (next', next:ns', finalmap)
                    Just ix -> let (next', ns', nextmap) = mapnames next ns curmap
                                in (next', ix:ns', nextmap)
                gmap' = M.insert name ids' gmap


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

flattenEitherVmap :: (a -> Either e [b]) -> V.Vector a -> Either e (V.Vector b)
flattenEitherVmap f vs = unfoldV <$> V.mapM f vs
    where
        unfoldV :: V.Vector [a] -> V.Vector a
        unfoldV v = V.unfoldr access (0, [])
            where
                access (!vi, x:xs) = Just (x, (vi,xs))
                access (!vi,[])
                    | vi >= V.length vs = Nothing
                    | otherwise = access (vi+1, V.unsafeIndex v vi)

getFeatureName (GffOther s) = s
getFeatureName _ = error "getFeatureName called for non-GffOther input"

loadGFF :: FilePath -> CountOpts -> NGLessIO Annotator
loadGFF gffFp opts = do
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "'..."]
        (_, amap,namemap,szmap) <- (conduitPossiblyCompressedFile gffFp
                $= CB.lines)
                $$& (readAnnotationOrDie
                =$= CL.filter (matchFeatures $ optFeatures opts)
                =$= (CL.fold insertg (0, M.empty, M.empty, M.empty)))

        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "' complete."]
        let (amap',headers) = reindex amap namemap
        return (GFFAnnotator amap' headers szmap)
    where
        readAnnotationOrDie :: C.Conduit B.ByteString NGLessIO GffLine
        readAnnotationOrDie = C.awaitForever $ \line ->
            unless (B8.head line == '#') $
                case readGffLine line of
                    Right g -> C.yield g
                    Left err -> throwError err
        insertg :: (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double) -> GffLine -> (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double)
        insertg (next, gmap, namemap, szmap) gline = (next', gmap', namemap', szmap')
            where
                header = B.concat [B8.pack (show $ gffType gline), "\t", gffId gline]
                (namemap', active, !next') = case M.lookup header namemap of
                    Just v -> (namemap, v, next)
                    Nothing -> (M.insert header next namemap, next, next+1)

                gmap' :: GFFAnnotationMap
                gmap' = M.alter insertg' (gffSeqId gline) gmap
                insertg' immap = Just $ IM.alter insertGffLine' asInterval (fromMaybe IM.empty immap)

                insertGffLine' Nothing  = Just [annot]
                insertGffLine' (Just vs) = Just (annot:vs)
                annot = (gffStrand gline, active)

                asInterval :: IM.Interval Int
                asInterval = IM.ClosedInterval (gffStart gline) (gffEnd gline)
                szmap' = M.alter (inserts1 gline) header szmap
        reindex :: GFFAnnotationMap -> M.Map B.ByteString Int -> (GFFAnnotationMap, [B.ByteString])
        reindex amap namemap = (M.map (IM.mapWithKey (const $ map reindexAI)) amap, headers)
            where
                headers = M.keys namemap
                reindexAI :: AnnotationInfo -> AnnotationInfo
                reindexAI (s, v) = (s, reindexIx v)

                reindexIx :: Int -> Int
                reindexIx ov = fromJust (M.lookup ov ix)
                ix = M.fromList $ map (\(i, (_,v)) -> (i,v)) $ zip [0..] (M.assocs namemap)

        gffSize :: GffLine -> Int
        gffSize g = gffEnd g - gffStart g

        inserts1 :: GffLine -> Maybe Double -> Maybe Double
        inserts1 g cur = Just $ convert (gffSize g) + fromMaybe 0.0 cur


annotateSamLine :: CountOpts -> GFFAnnotationMap -> SamLine -> [Int]
annotateSamLine opts amap samline = case M.lookup rname amap of
        Nothing -> []
        Just im ->  map snd . (if optKeepAmbiguous opts then id else filterAmbiguous)
                    $ (optIntersectMode opts) im asStrand (sStart, sEnd)
    where
        rname = samRName samline
        sStart = samPos samline
        sEnd   = sStart + samLength samline - 1
        asStrand :: GffStrand
        asStrand = if optStrandSpecific opts
                        then if isPositive samline then GffPosStrand else GffNegStrand
                        else GffUnStranded


matchStrand :: GffStrand -> GffStrand -> Bool
matchStrand GffUnStranded _ = True
matchStrand _ GffUnStranded = True
matchStrand a b = a == b

filterAmbiguous  :: [AnnotationInfo] -> [AnnotationInfo]
filterAmbiguous [] = []
filterAmbiguous ms
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

