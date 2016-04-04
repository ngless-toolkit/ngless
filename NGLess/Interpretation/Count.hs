{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections #-}
module Interpretation.Count
    ( executeCount
    , Annotator(..)
    , CountOpts(..)
    , AnnotationMode(..)
    , AnnotationIntersectionMode(..)
    , MMMethod(..)
    , annotateSamLine
    , annotationRule
    , loadAnnotator
    , loadFunctionalMap
    , matchFeatures
    , performCount
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Double.Conversion.ByteString as D
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Vector.Algorithms.Intro (sort)

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Strict.Tuple (Pair(..))

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit (($=), (=$), (=$=), ($$+), ($$+-), ($$))
import           Data.Conduit.Async (buffer, (=$=&), ($$&))

import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import Data.List                (foldl1')
import GHC.Conc                 (getNumCapabilities)
import Control.DeepSeq          (NFData(..))
import Control.Error            (note)
import Data.Maybe

import System.IO                (hClose)
import Data.Convertible         (convert)

import Data.GFF
import Data.Sam (SamLine(..), samLength, isAligned, isPositive, readSamGroupsC)
import FileManagement (openNGLTempFile)
import ReferenceDatabases
import NGLess.NGError
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


data RefSeqInfo = RefSeqInfo {-# UNPACK #-} !B.ByteString {-# UNPACK #-} !Double
    deriving (Eq, Show)

instance NFData RefSeqInfo where
    rnf !_ = ()

instance Ord RefSeqInfo where
    compare (RefSeqInfo s0 _) (RefSeqInfo s1 _) = compare s0 s1

compareShortLong  :: RefSeqInfo -> B.ByteString -> Ordering
compareShortLong (RefSeqInfo short _) long = compare short long

data Annotator =
                SeqNameAnnotator (Maybe (V.Vector RefSeqInfo))
                | GFFAnnotator GFFAnnotationMap [B.ByteString] FeatureSizeMap
                | GeneMapAnnotator GeneMapAnnotation [B.ByteString] FeatureSizeMap
    deriving (Eq, Show)
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m
    rnf (GFFAnnotator amap headers szmap) = rnf amap `seq` rnf headers `seq` rnf szmap
    rnf (GeneMapAnnotator amap headers szmap) = rnf amap `seq` rnf headers `seq` rnf szmap

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> Either NGError [Int]
annotateReadGroup opts ann samlines = listNub <$> case ann of
        SeqNameAnnotator Nothing -> throwShouldNotOccur ("Incomplete annotator used" :: String)
        SeqNameAnnotator (Just szmap) -> mapMaybeM (getID szmap) samlines
        GFFAnnotator amap _ _ -> return . concatMap (annotateSamLine opts amap) $ samlines
        GeneMapAnnotator amap _ _ -> return . concatMap (mapAnnotation1 amap) $ samlines
    where
        getID :: V.Vector RefSeqInfo -> SamLine -> Either NGError (Maybe Int)
        getID szmap sr@SamLine{samRName = rname }
            | isAligned sr = case binarySearchByExact compareShortLong szmap rname of
                    Nothing -> throwDataError ("Unknown sequence id: " ++ show rname)
                    ix -> return ix
        getID _ _ = Right Nothing
        mapAnnotation1 :: GeneMapAnnotation ->  SamLine -> [Int]
        mapAnnotation1 amap samline = fromMaybe [] $ M.lookup (samRName samline) amap

finishAnnotator :: Annotator -> V.Vector RefSeqInfo -> Annotator
finishAnnotator SeqNameAnnotator{} idszmap = SeqNameAnnotator (Just idszmap)
finishAnnotator ann _ = ann

annSizeOf :: Annotator -> B.ByteString -> Either NGError Double
annSizeOf (SeqNameAnnotator Nothing) _ = throwShouldNotOccur ("Using unloaded annotator" :: String)
annSizeOf (SeqNameAnnotator (Just ix)) name = (\(RefSeqInfo _ s) -> s)  <$> note (NGError DataError "unknown error") (binaryFindBy compareShortLong ix name)
annSizeOf (GFFAnnotator _ _ szmap) name = annSizeOf' name szmap
annSizeOf (GeneMapAnnotator _ _ szmap) name = annSizeOf' name szmap
annSizeOf' name ix = case M.lookup name ix of
    Just s -> return s
    Nothing -> throwShouldNotOccur ("Header does not exist in sizes: "++show name)


annEnumerate :: Annotator -> [(B.ByteString, Int)]
annEnumerate (SeqNameAnnotator Nothing) = error "Using unfinished annotator"
annEnumerate (SeqNameAnnotator (Just ix)) = V.toList . flip V.imap ix $ \i (RefSeqInfo n _) -> (n, i)
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


performCount1Pass :: VUM.IOVector Double -> MMMethod -> C.Sink (VU.Vector Int, V.Vector [Int]) NGLessIO [V.Vector [Int]]
performCount1Pass mcounts method = loop []
    where
        loop acc = C.await >>= \case
            Nothing -> return acc
            Just (singles,mms) -> case method of
                MMCountAll -> do
                    liftIO $ incrementAll mcounts singles
                    loop acc
                MM1OverN -> do
                    liftIO $ incrementAll mcounts singles
                    forM_ mms (liftIO . increment1OverN mcounts)
                    loop acc
                MMDist1 ->  do
                    liftIO $ incrementAll mcounts singles
                    loop $ if V.length mms > 0
                                then mms:acc
                                else acc

-- | Equivalent to Python's enumerate
enumerate :: (Monad m) => C.Conduit a m (Int, a)
enumerate = loop 0
    where
        loop !n = awaitJust $ \v -> C.yield (n, v) >> loop (n+1)

annSamHeaderParser :: Int -> Annotator -> C.Sink ByteLine NGLessIO (V.Vector RefSeqInfo)
annSamHeaderParser mapthreads SeqNameAnnotator{} = do
    let seqNameSize :: (Int, ByteLine) -> Either NGError RefSeqInfo
        seqNameSize (n, ByteLine h) = case B8.split '\t' h of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return $! RefSeqInfo (B.drop 3 seqname) (convert size)
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")

        buildVector :: V.Vector (Int, ByteLine) -> Either NGError (V.Vector RefSeqInfo)
        buildVector pairs = case V.mapM seqNameSize pairs of
            Right v -> let !r = V.create $ do
                                        v' <- V.unsafeThaw v
                                        sort v'
                                        return v'
                        in Right r
            err -> err
    CL.filter (B.isPrefixOf "@SQ\tSN:" . unwrapByteLine)
        =$= enumerate
        =$= C.conduitVector 16384
        =$= asyncMapEitherC mapthreads buildVector
        =$= (getVector <$> CL.fold mergeVM (VectorMerge []))
annSamHeaderParser _ _ = C.sinkNull >> return V.empty

{- This is a simple type that just accumulates sorted vectors into increasingly large blocks -}
newtype VectorMerge a = VectorMerge [Pair Int (V.Vector a)]

mergeVM :: (Ord a) => VectorMerge a -> V.Vector a -> VectorMerge a
mergeVM (VectorMerge start) newv = VectorMerge $! mergeVM' 1 start newv
    where
        mergeVM' w [] !v = [w :!: v]
        mergeVM' w vm@((w' :!: v'):rs) v
            | w == w' = mergeVM' (w+1) rs (mergeV v v')
            | otherwise = (w :!: v):vm

getVector :: (Ord a) => VectorMerge a -> V.Vector a
getVector (VectorMerge vm) = loop vm V.empty
    where
        loop [] v = v
        loop ((_ :!: v'):rs) v = loop rs (mergeV v v')

mergeV :: (Ord a) => V.Vector a -> V.Vector a -> V.Vector a
mergeV v0 v1
    | V.length v0 == 0 = v1
    | V.length v1 == 0 = v0
    | otherwise = V.create $ do
        let n0 = V.length v0
            n1 = V.length v1
            n = n0 + n1
        r <- VM.new n

        let loop p0 p1 pr
                | p0 < n0 && p1 < n1 = case compare ((V.!) v0 p0) ((V.!) v1 p1) of
                    LT -> vectorCopyElem' v0 p0 pr >> loop (p0 + 1) p1      (pr + 1)
                    _  -> vectorCopyElem' v1 p1 pr >> loop  p0     (p1 + 1) (pr + 1)
                | p0 < n0 = copyRest v0 p0 pr n0
                | p1 < n1 = copyRest v1 p1 pr n1
                | otherwise = return ()
            copyRest v s pr n
                | s == n = return ()
                | otherwise = vectorCopyElem' v s pr >> copyRest v (s + 1) (pr + 1) n

            vectorCopyElem' v !i !j = do
                val <- V.indexM v i
                VM.write r j val
        loop 0 0 0
        return r


listNub :: (Ord a) => [a] -> [a]
listNub = S.toList . S.fromList

splitSingles :: MMMethod -> V.Vector [Int] -> (VU.Vector Int, V.Vector [Int])
splitSingles method values = (singles, mms)
    where
        singles = VU.create $ do
            v <- VU.unsafeThaw $ VU.unfoldr getsingle1 0
            sort v -- sorting is completely unnecessary for correctness, but improves cache performance as close-by indices will be accessed together
            return v
        getsingle1 :: Int -> Maybe (Int, Int)
        getsingle1 ix = do
            v <- values V.!? ix
            case v of
                [v] -> return (v, ix + 1)
                _ -> getsingle1 (ix + 1)
        mms = if method /= MMDist1
                then V.empty
                else V.filter larger1 values
        larger1 []  = False
        larger1 [_] = False
        larger1 _   = True

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
    toDistribute <-
            samcontent
                $$+- readSamGroupsC
                =$= C.conduitVector 2048
                =$= asyncMapEitherC mapthreads (liftM (splitSingles method) . V.mapM (annotateReadGroup opts annotator))
                =$= performCount1Pass mcounts method

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

        let nlB :: BB.Builder
            nlB = BB.word8 10
            tabB :: BB.Builder
            tabB = BB.word8 9
            encode1 :: Int -> BB.Builder
            encode1 !i = let
                        hn = (V.!) headers i
                        v = (VU.!) result i
                    in if v >= optMinCount opts
                                then mconcat [BB.byteString hn, tabB, BB.byteString (D.toShortest v), nlB]
                                else mempty

            n = VU.length result
        mapM_ (BB.hPutBuilder hout. encode1) [0 .. n - 1]
        hClose hout
    return newfp



incrementAll :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAll counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

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
        VUM.unsafeModify counts (/ s) i

distributeMM indices current fractionResult = VU.create $ do
    ncounts <- VU.thaw current -- note that thaw performs a copy
    forM_ indices $ \vss -> forM_ vss $ \vs -> do
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
        singleFeature = length (optFeatures opts) == 1
        readAnnotationOrDie :: C.Conduit B.ByteString NGLessIO GffLine
        readAnnotationOrDie = C.awaitForever $ \line ->
            unless (B8.head line == '#') $
                case readGffLine line of
                    Right g -> C.yield g
                    Left err -> throwError err
        insertg :: (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double) -> GffLine -> (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double)
        insertg (next, gmap, namemap, szmap) gline = (next', gmap', namemap', szmap')
            where
                header = if singleFeature
                                then gffId gline
                                else B.concat [B8.pack (show $ gffType gline), "\t", gffId gline]
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
                ix = M.fromList $ zip (M.elems namemap) [0..]

        gffSize :: GffLine -> Int
        gffSize g = (gffEnd g - gffStart g) + 1 -- gff format is inclusive at both ends!

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


filterAmbiguous  :: [AnnotationInfo] -> [AnnotationInfo]
filterAmbiguous [] = []
filterAmbiguous ms
    | allSame (snd <$> ms) = [head ms]
    | otherwise = [] -- ambiguous: discard

filterStrand :: GffStrand -> IM.IntervalMap Int [AnnotationInfo] -> IM.IntervalMap Int [AnnotationInfo]
filterStrand GffUnStranded = id
filterStrand strand = IM.filterWithKey filterEmpty . IM.mapWithKey matchStrand
    where
        matchStrand _ = filter (matchStrand' . fst)
        matchStrand' :: GffStrand -> Bool
        matchStrand' GffUnStranded = True
        matchStrand' s = strand == s
        filterEmpty _ = not . null

union :: AnnotationRule
union im strand (sS, sE) =  concat . IM.elems . filterStrand strand . IM.intersecting im $ IM.ClosedInterval sS sE

intersection_strict :: AnnotationRule
intersection_strict im strand (sS, sE) = intersection' im'
    where im' = map (filterStrand strand . IM.containing im) [sS..sE]

intersection_non_empty :: AnnotationRule
intersection_non_empty im strand (sS, sE) = intersection' im'
    where
        im' = filter (not . null) .  map (filterStrand strand . IM.containing subim) $ [sS..sE]
        subim = IM.intersecting im (IM.ClosedInterval sS sE)

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

