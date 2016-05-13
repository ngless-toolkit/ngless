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
    , RefSeqInfo(..)
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

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit (($=), (=$), (=$=), ($$+), ($$+-))
import           Data.Conduit.Async (($$&))

import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import Data.List                (foldl1', unfoldr)
import GHC.Conc                 (getNumCapabilities)
import Control.DeepSeq          (NFData(..))
import Control.Error            (note)
import Data.Maybe

import System.IO                (hClose)
import Data.Convertible         (convert)

import Data.GFF
import Data.Sam (SamLine(..), samLength, isAligned, isPositive, readSamGroupsC')
import FileManagement (openNGLTempFile)
import ReferenceDatabases
import NGLess.NGError
import Language
import Output
import NGLess

import Utils.Utils
import Utils.Vector
import Utils.Conduit
import Utils.Samtools
import Utils.Suggestion
import qualified Utils.IntGroups as IG


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


data RefSeqInfo = RefSeqInfo
                        { rsiName :: {-# UNPACK #-} !B.ByteString
                        , rsiSize :: {-# UNPACK #-} !Double
                        } deriving (Eq, Show)
instance NFData RefSeqInfo where
    rnf !_ = ()

instance Ord RefSeqInfo where
    compare (RefSeqInfo s0 _) (RefSeqInfo s1 _) = compare s0 s1

compareShortLong  :: RefSeqInfo -> B.ByteString -> Ordering
compareShortLong (RefSeqInfo short _) long = compare short long

data Annotator =
                SeqNameAnnotator (Maybe (V.Vector RefSeqInfo))
                | GFFAnnotator GFFAnnotationMap [B.ByteString] FeatureSizeMap
                | GeneMapAnnotator GeneMapAnnotation (V.Vector RefSeqInfo)
    deriving (Eq, Show)
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m
    rnf (GFFAnnotator amap headers szmap) = rnf amap `seq` rnf headers `seq` rnf szmap
    rnf (GeneMapAnnotator amap szmap) = rnf amap `seq` rnf szmap

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> Either NGError [Int]
annotateReadGroup opts ann samlines = listNub <$> case ann of
        SeqNameAnnotator Nothing -> throwShouldNotOccur "Incomplete annotator used"
        SeqNameAnnotator (Just szmap) -> mapMaybeM (getID szmap) samlines
        GFFAnnotator amap _ _ -> return . concatMap (annotateSamLine opts amap) $ samlines
        GeneMapAnnotator amap _ -> return . concatMap (mapAnnotation1 amap) $ samlines
    where
        getID :: V.Vector RefSeqInfo -> SamLine -> Either NGError (Maybe Int)
        getID szmap sr@SamLine{samRName = rname }
            | isAligned sr = case binarySearchByExact compareShortLong szmap rname of
                    Nothing -> throwDataError ("Unknown sequence id: " ++ show rname)
                    ix -> return ix
        getID _ _ = Right Nothing
        mapAnnotation1 :: GeneMapAnnotation ->  SamLine -> [Int]
        mapAnnotation1 amap samline = fromMaybe [] $ M.lookup (samRName samline) amap

annSizeOf :: Annotator -> B.ByteString -> Either NGError Double
annSizeOf (SeqNameAnnotator Nothing) _ = throwShouldNotOccur "Using unloaded annotator"
annSizeOf (SeqNameAnnotator (Just ix)) name = annSizeOfInRSVector name ix
annSizeOf (GFFAnnotator _ _ szmap) name = annSizeOf' name szmap
annSizeOf (GeneMapAnnotator _ ix) name = annSizeOfInRSVector name ix
annSizeOfInRSVector name ix = rsiSize <$> note (NGError DataError "Could not find size of item") (binaryFindBy compareShortLong ix name)
annSizeOf' name ix = case M.lookup name ix of
    Just s -> return s
    Nothing -> throwShouldNotOccur ("Header does not exist in sizes: "++show name)


annEnumerate :: Annotator -> [(B.ByteString, Int)]
annEnumerate (SeqNameAnnotator Nothing)   = error "Using unfinished annotator"
annEnumerate (SeqNameAnnotator (Just ix)) = enumerateRSVector ix
annEnumerate (GeneMapAnnotator _ ix)      = enumerateRSVector ix
annEnumerate (GFFAnnotator _ headers _)   = zip headers [0..]
enumerateRSVector ix = V.toList . flip V.imap ix $ \i rs -> (rsiName rs, i)

annSize :: Annotator -> Int
annSize (SeqNameAnnotator (Just ix)) = V.length ix
annSize (GeneMapAnnotator _ ix) = V.length ix
annSize ann = length (annEnumerate ann)

methodFor "1overN" = return MM1OverN
methodFor "dist1" = return MMDist1
methodFor "all1" = return MMCountAll
methodFor other = throwShouldNotOccur ("Unexpected multiple method " ++ T.unpack other)

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
        _ -> throwShouldNotOccur "executeAnnotation: TYPE ERROR"

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
    amode <- annotationMode (optFeatures opts) refinfo (T.unpack <$> mocatMap) (T.unpack <$> gffFile)
    annotator <- loadAnnotator amode opts
    NGOCounts <$> performCount samfp rname annotator opts
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


loadAnnotator :: AnnotationMode -> CountOpts -> NGLessIO Annotator
loadAnnotator AnnotateSeqName _ = return $ SeqNameAnnotator Nothing
loadAnnotator (AnnotateGFF gf) opts = loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = loadFunctionalMap mm (map getFeatureName $ optFeatures opts)


performCount1Pass :: VUM.IOVector Double -> MMMethod -> C.Sink (VU.Vector Int, IG.IntGroups) NGLessIO [IG.IntGroups]
performCount1Pass mcounts MMCountAll = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (incrementAll2 mcounts)
    return []
performCount1Pass mcounts MM1OverN = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (increment1OverN mcounts)
    return []
performCount1Pass mcounts MMDist1 = loop []
    where
        loop :: [IG.IntGroups] -> C.Sink (VU.Vector Int, IG.IntGroups) NGLessIO [IG.IntGroups]
        loop acc = C.await >>= \case
            Nothing -> return acc
            Just (singles, mms) ->  do
                    liftIO $ incrementAll mcounts singles
                    loop $ if not (IG.null mms)
                                then mms:acc
                                else acc

-- | Equivalent to Python's enumerate
enumerate :: (Monad m) => C.Conduit a m (Int, a)
enumerate = loop 0
    where
        loop !n = awaitJust $ \v -> C.yield (n, v) >> loop (n+1)

annSamHeaderParser :: Int -> Annotator -> CountOpts -> C.Sink ByteLine NGLessIO Annotator
annSamHeaderParser mapthreads ann opts = case ann of
        SeqNameAnnotator Nothing -> do
            chunks <- lineGroups
                =$= asyncMapEitherC mapthreads (V.mapM seqNameSize)
                =$= CL.fold (flip (:)) []
            vsorted <- liftIO $ do
                v <- V.unsafeThaw (V.concat chunks)
                sortParallel mapthreads v
                V.unsafeFreeze v
            return $! SeqNameAnnotator (Just vsorted)
        GeneMapAnnotator gmap isizes
            | optNormSize opts -> do
                msizes <- liftIO $ V.thaw isizes
                chunks <- lineGroups
                    =$= asyncMapEitherC mapthreads (liftM flattenVs . V.mapM (indexUpdates gmap))
                    =$= CL.mapM_ (liftIO . updateSizes msizes)
                GeneMapAnnotator gmap <$> liftIO (V.unsafeFreeze msizes)
        _ -> C.sinkNull >> return ann
    where
        lineGroups = CL.filter (B.isPrefixOf "@SQ\tSN:" . unwrapByteLine)
                    =$= enumerate
                    =$= C.conduitVector 32768
        flattenVs :: VU.Unbox a => V.Vector [a] -> VU.Vector a
        flattenVs chunks = VU.unfoldr getNext (0,[])
            where
                getNext (!vi, v:vs) = Just (v, (vi,vs))
                getNext (vi,[])
                    | vi >= V.length chunks = Nothing
                    | otherwise = getNext (vi + 1, chunks V.! vi)

        updateSizes :: VM.IOVector RefSeqInfo -> VU.Vector (Int,Double) -> IO ()
        updateSizes msizes updates =
            VU.forM_ updates $ \(ix,val) ->
                VM.modify msizes (rsiAdd val) ix
        rsiAdd !val (RefSeqInfo n cur) = RefSeqInfo n (cur + val)
        indexUpdates :: GeneMapAnnotation -> (Int, ByteLine) -> Either NGError [(Int, Double)]
        indexUpdates gmap line = do
            RefSeqInfo seqid val <- seqNameSize line
            case M.lookup seqid gmap of
                Nothing -> return []
                Just ixs -> return [(ix,val) | ix <- ixs]
        seqNameSize :: (Int, ByteLine) -> Either NGError RefSeqInfo
        seqNameSize (n, ByteLine h) = case B8.split '\t' h of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return $! RefSeqInfo (B.drop 3 seqname) (convert size)
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")


listNub :: (Ord a) => [a] -> [a]
listNub = S.toList . S.fromList

splitSingles :: MMMethod -> V.Vector [Int] -> (VU.Vector Int, IG.IntGroups)
splitSingles method values = (singles, mms)
    where
        singles = VU.create $ do
            v <- VU.unsafeThaw $ VU.unfoldr getsingle1 0
            sort v -- sorting is completely unnecessary for correctness, but improves cache performance as close-by indices will be accessed together
            return v
        getsingle1 :: Int -> Maybe (Int, Int)
        getsingle1 ix = do
            vs <- values V.!? ix
            case vs of
                [v] -> return (v, ix + 1)
                _ -> getsingle1 (ix + 1)
        mms = IG.fromList (filter larger1 (V.toList values))
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

    (samcontent, annotator) <-
        samBamConduit samfp
            =$= linesC
            $$+ C.takeWhile ((=='@') . B8.head . unwrapByteLine)
            =$= annSamHeaderParser mapthreads annotator0 opts
    let n_entries = annSize annotator

    outputListLno' TraceOutput ["Loaded headers (", show n_entries, " headers); starting parsing/distribution."]
    mcounts <- liftIO $ VUM.replicate n_entries (0.0 :: Double)
    toDistribute <-
            samcontent
                $$+- readSamGroupsC' mapthreads
                =$= asyncMapEitherC mapthreads (liftM (splitSingles method) . V.mapM (annotateReadGroup opts annotator))
                =$= performCount1Pass mcounts method
    sizes <- if optNormSize opts || method == MMDist1
                then do
                    sizes <- liftIO $ VUM.new n_entries
                    forM_ (annEnumerate annotator) $ \(name,i) -> do
                        s <- runNGLess $ annSizeOf annotator name
                        liftIO $ VUM.write sizes i s
                    return sizes
                else liftIO $ VUM.new 0

    raw_counts <- if method == MMDist1
                    then liftIO (VUM.clone mcounts)
                    else return mcounts
    when (optNormSize opts || method == MMDist1) $ do
        normalizeCounts mcounts sizes
    counts <- liftIO $ VU.unsafeFreeze mcounts

    result <-
        if method /= MMDist1
            then return counts
            else do
                outputListLno' TraceOutput ["Counts (second pass)..."]
                distributeMM toDistribute counts raw_counts False
                when (optNormSize opts) $ do
                    normalizeCounts raw_counts sizes
                liftIO $ VU.unsafeFreeze raw_counts

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

incrementAll2 :: VUM.IOVector Double -> [Int] -> IO ()
incrementAll2 counts vis = forM_ vis $ \vi -> unsafeIncrement counts vi

increment1OverN :: VUM.IOVector Double -> [Int] -> IO ()
increment1OverN counts vis = forM_ vis $ \vi -> unsafeIncrement' counts vi oneOverN
    where
        oneOverN :: Double
        oneOverN = 1.0 / convert (length vis)

normalizeCounts :: VUM.IOVector Double -> VUM.IOVector Double -> NGLessIO ()
normalizeCounts counts sizes = do
    let n = VUM.length counts
        n' = VUM.length sizes
    unless (n == n') $
        throwShouldNotOccur ("Counts vector is of size " ++ show n ++ ", but sizes if of size " ++ show n')
    forM_ [0 .. n - 1] $ \i -> liftIO $ do
        s <- VUM.read sizes i
        VUM.unsafeModify counts (/ s) i

distributeMM indices normed ncounts fractionResult = liftIO $ do
    forM_ indices $ \vss -> IG.forM_ vss $ \vs -> do
        let cs = map (VU.unsafeIndex normed) vs
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


loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO Annotator
loadFunctionalMap fname columns = do
        outputListLno' InfoOutput ["Loading map file ", fname]
        (resume, [headers]) <- (CB.sourceFile fname =$ CB.lines)
                $$+ (CL.isolate 1 =$= CL.map (B8.split '\t') =$ CL.consume)
        cis <- runNGLess $ lookUpColumns headers
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        (_,gmap,namemap) <- resume
                $$+- C.conduitVector 8192
                =$= asyncMapEitherC mapthreads (V.mapM (selectColumns cis . B8.split '\t'))
                =$ CL.fold (V.foldl' inserts1) (0, M.empty,M.empty)
        outputListLno' TraceOutput ["Loading of map file '", fname, "' complete"]
        return $ GeneMapAnnotator (reindex gmap namemap) (V.fromList [RefSeqInfo n 0.0 | n <- M.keys namemap])
    where
        reindex :: M.Map B.ByteString [Int] -> M.Map B.ByteString Int -> M.Map B.ByteString [Int]
        reindex gmap namemap = M.map (map reindex1) gmap
            where
                reindex1 :: Int -> Int
                reindex1 = fromJust . flip M.lookup remap
                remap = M.fromList (zip (M.elems namemap) [0..])
        inserts1 :: (Int, M.Map B.ByteString [Int], M.Map B.ByteString Int) -> (B.ByteString, [B.ByteString]) -> (Int, M.Map B.ByteString [Int], M.Map B.ByteString Int)
        inserts1 (next, gmap, namemap) (name, ids) = (next', gmap', namemap')
            where
                (next', ids', namemap') = mapnames next (flattenIds ids) namemap

                flattenIds = flatten . map (B8.split ',')

                flatten :: [[a]] -> [a]
                flatten = unfoldr flatten'
                    where
                        flatten' [] = Nothing
                        flatten' ([]:xs) = flatten' xs
                        flatten' ((v:vs):xs) = Just (v, vs:xs)

                mapnames next [] curmap = (next, [], curmap)
                mapnames next (n:ns) curmap = case M.lookup n curmap of
                    Nothing -> let  nextmap = M.insert n next curmap
                                    (next', ns', finalmap) = mapnames (next+1) ns nextmap
                                in (next', next:ns', finalmap)
                    Just ix -> let (next', ns', nextmap) = mapnames next ns curmap
                                in (next', ix:ns', nextmap)
                gmap' = M.insert name ids' gmap


        lookUpColumns :: [B.ByteString] -> NGLess [Int]
        lookUpColumns [] = throwDataError ("Loading functional map file '" ++ fname ++ "': Header line missing!")
        lookUpColumns headers = mapM (lookUpColumns' $ M.fromList (zip (tail headers) [0..])) columns
        lookUpColumns' :: M.Map B8.ByteString Int -> B8.ByteString -> NGLess Int
        lookUpColumns' colmap col = note notfounderror $ M.lookup col colmap
            where
                notfounderror = NGError DataError errormsg
                errormsg = concat (["Could not find column '", B8.unpack $ col, "'."]
                                ++ case findSuggestion (T.pack $ B8.unpack col) (map (T.pack . B8.unpack) $ M.keys colmap) of
                                        Just (Suggestion valid reason) -> [" Did you mean '", T.unpack valid, "' (", T.unpack reason, ")?"]
                                        Nothing -> [])
        selectColumns :: [Int] -> [B.ByteString] -> NGLess (B.ByteString, [B.ByteString])
        selectColumns cols (gene:mapped) = (gene,) <$> selectIds cols (zip [0..] mapped)
        selectColumns _ [] = throwDataError ("Loading functional map file '" ++ fname ++ "': empty line.")

        selectIds :: [Int] -> [(Int, B.ByteString)] -> NGLess [B.ByteString]
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


annotationMode :: [GffType] -> Maybe T.Text -> Maybe FilePath -> Maybe FilePath -> NGLessIO AnnotationMode
annotationMode _ _ (Just _) (Just _) = throwScriptError "Cannot simmultaneously pass a gff_file and an annotation_file for annotate() function"
annotationMode [GffOther "seqname"] _ _ _ = return AnnotateSeqName
annotationMode _ _ (Just r) _ = return (AnnotateFunctionalMap r)
annotationMode _ _ _ (Just g) = return (AnnotateGFF g)
annotationMode _ (Just ref) Nothing Nothing = do
    outputListLno' InfoOutput ["Annotate with default GFF: ", show ref]
    ReferenceFilePaths _ mgffpath _ <- ensureDataPresent ref
    case mgffpath of
        Just gffpath -> return $! AnnotateGFF gffpath
        Nothing -> throwScriptError ("Could not find annotation file for '" ++ T.unpack ref ++ "'")
annotationMode _ _ _ _ =
            throwShouldNotOccur "For counting, you must do one of\n1. use seqname mode\n2. pass in a GFF file using the argument 'gff_file'\n3. pass in a gene map using the argument 'functional_map'"

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

