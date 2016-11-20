{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP #-}
module Interpretation.Count
    ( executeCount
    , executeCountFile
    , Annotator(..)
    , CountOpts(..)
    , AnnotationMode(..)
    , AnnotationIntersectionMode(..)
    , MMMethod(..)
    , annotationRule
    , loadAnnotator
    , loadFunctionalMap
    , performCount
    , RefSeqInfo(..)
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA

import qualified Data.IntervalMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit ((=$), (=$=), ($$+), ($$+-))
import qualified Data.Strict.Tuple as TU
import           Data.Strict.Tuple (Pair(..))

import Control.Monad
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Except     (throwError)
import Data.List                (foldl1', foldl', sort, zip4)
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
import FileOrStream
import Language
import Output
import NGLess

import Utils.Utils
import Utils.Vector
import Utils.Conduit
import Utils.Samtools
import Utils.Suggestion
import qualified Utils.IntGroups as IG

#ifndef WINDOWS
import Data.Double.Conversion.ByteString (toShortest)
#else
-- On Windows, double-conversion is problematic, so fall back on a basic
-- implementation
-- See https://github.com/bos/double-conversion/issues/7
toShortest :: Double -> B.ByteString
toShortest = B8.pack . show
#endif

{- Implementation of count()
 -
 - The main function is performCount which loops over mapped read groups
 -. annotating them with an Annotator.
 -}
-- GFFAnnotationMap maps from `References` (e.g., chromosomes) to positions to (strand/feature-id)
type AnnotationInfo = Pair GffStrand Int
type GffIMMap = IM.IntervalMap Int [AnnotationInfo]
type GFFAnnotationMap = M.Map B.ByteString GffIMMap
type AnnotationRule = GffIMMap -> GffStrand -> (Int, Int) -> [AnnotationInfo]

type GeneMapAnnotation = M.Map B8.ByteString [Int]

type FeatureSizeMap = M.Map B.ByteString Double

data MMMethod = MMCountAll | MM1OverN | MMDist1 | MMUniqueOnly
    deriving (Eq, Show)


minDouble :: Double
minDouble = (2.0 :: Double) ^^ fst (floatRange (1.0 :: Double))

data CountOpts =
    CountOpts
    { optFeatures :: [B.ByteString] -- ^ list of features to condider
    , optIntersectMode :: AnnotationRule
    , optStrandSpecific :: !Bool
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
    compare RefSeqInfo{ rsiName = n0 } RefSeqInfo{ rsiName = n1 } = compare n0 n1


compareShortLong  :: RefSeqInfo -> B.ByteString -> Ordering
compareShortLong (RefSeqInfo short _) long = compare short long

data Annotator =
                SeqNameAnnotator (Maybe (V.Vector RefSeqInfo)) -- ^ Just annotate by sequence names
                | GFFAnnotator GFFAnnotationMap [B.ByteString] FeatureSizeMap -- ^ map reference regions to features + feature sizes
                | GeneMapAnnotator GeneMapAnnotation (V.Vector RefSeqInfo) -- ^ map reference (gene names) to indices, indexing into the vector of refseqinfo
    deriving (Eq, Show)
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m
    rnf (GFFAnnotator amap headers szmap) = amap `seq` rnf headers `seq` rnf szmap -- amap is already strict
    rnf (GeneMapAnnotator amap szmap) = rnf amap `seq` rnf szmap

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> Either NGError [Int]
annotateReadGroup opts ann samlines = listNub <$> case ann of
        SeqNameAnnotator Nothing -> throwShouldNotOccur "Incomplete annotator used"
        SeqNameAnnotator (Just szmap) -> mapMaybeM (getID szmap) samlines
        GFFAnnotator amap _ _ -> return . concatMap (annotateSamLineGFF opts amap) $ samlines
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

-- Number of elements
annSize :: Annotator -> Int
annSize (SeqNameAnnotator (Just ix)) = V.length ix
annSize (GeneMapAnnotator _ ix) = V.length ix
annSize ann = length (annEnumerate ann)

methodFor "1overN" = return MM1OverN
methodFor "dist1" = return MMDist1
methodFor "all1" = return MMCountAll
methodFor "unique_only" = return MMUniqueOnly
methodFor other = throwShouldNotOccur ("Unexpected multiple method " ++ T.unpack other)


{- We define the type AnnotationIntersectionMode mainly to facilitate tests,
 - which depend on being able to write code such as
 -
 -      annotationRule IntersectUnion
 -}
data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)

annotationRuleFor "union" = return IntersectUnion
annotationRuleFor "intersection_strict" = return IntersectUnion
annotationRuleFor "intersection_non_empty" = return IntersectNonEmpty
annotationRuleFor m = throwScriptError (concat ["Unexpected annotation mode (", show m, ")."])

annotationRule :: AnnotationIntersectionMode -> AnnotationRule
annotationRule IntersectUnion = union
annotationRule IntersectStrict = intersection_strict
annotationRule IntersectNonEmpty = intersection_non_empty


executeCountFile :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCountFile (NGOString st) _ = return $ NGOCounts (File (T.unpack st))
executeCountFile other _ = throwScriptError ("Unexpected argument to countfile(): expected str, got " ++ show other)

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOMappedReadSet rname istream refinfo) args = do
    minCount <- lookupIntegerOrScriptErrorDef (return 0) "count argument parsing" "min" args
    method <- methodFor =<< lookupSymbolOrScriptErrorDef (return "dist1")
                                    "multiple argument to count " "multiple" args
    strand_specific <- lookupBoolOrScriptErrorDef (return False) "annotation function" "strand" args
    mocatMap <- lookupFilePath "functional_map argument to count()" "functional_map" args
    gffFile <- lookupFilePath "gff_file argument to count()" "gff_file" args
    normSize <- lookupBoolOrScriptErrorDef (return False) "count function" "norm" args
    discardZeros <- lookupBoolOrScriptErrorDef (return False) "count argument parsing" "discard_zeros" args
    m <- (liftM annotationRule . annotationRuleFor) =<< lookupSymbolOrScriptErrorDef (return "union") "mode argument to count" "mode" args
    delim <- T.encodeUtf8 <$> lookupStringOrScriptErrorDef (return "\t") "count hidden argument (should always be valid)" "__delim" args

    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOSymbol f) -> return [f]
        Just (NGOList feats') -> mapM (stringOrTypeError "annotation features argument") feats'
        _ -> throwShouldNotOccur "executeAnnotation: TYPE ERROR"
    samfp <- asFile istream
    let opts = CountOpts
            { optFeatures = map (B8.pack . T.unpack) fs
            , optIntersectMode = m
            , optStrandSpecific = strand_specific
            , optMinCount = if discardZeros
                                then minDouble
                                else fromInteger minCount
            , optMMMethod = method
            , optDelim = delim
            , optNormSize = normSize
            }
    amode <- annotationMode (optFeatures opts) refinfo (T.unpack <$> mocatMap) (T.unpack <$> gffFile)
    annotators <- loadAnnotator amode opts
    NGOCounts . File <$> performCount samfp rname annotators opts
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


loadAnnotator :: AnnotationMode -> CountOpts -> NGLessIO [Annotator]
loadAnnotator AnnotateSeqName _ = return [SeqNameAnnotator Nothing]
loadAnnotator (AnnotateGFF gf) opts = loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = loadFunctionalMap mm (optFeatures opts)


performCount1Pass :: MMMethod -> VUM.IOVector Double -> C.Sink (VU.Vector Int, IG.IntGroups) NGLessIO [IG.IntGroups]
performCount1Pass MMUniqueOnly mcounts = do
    C.awaitForever $ \(singles, _) -> liftIO (incrementAll mcounts singles)
    return []
performCount1Pass MMCountAll mcounts = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (incrementAllV mcounts)
    return []
performCount1Pass MM1OverN mcounts = do
    C.awaitForever $ \(singles, mms) -> liftIO $ do
        incrementAll mcounts singles
        IG.forM_ mms (increment1OverN mcounts)
    return []
performCount1Pass MMDist1 mcounts = loop []
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


-- | This is a version of C.sequenceSinks which optimizes the case where a
-- single element is passed (it makes a small, but noticeable difference in
-- benchmarking)
sequenceSinks :: (Monad m) => [C.Sink a m b] -> C.Sink a m [b]
sequenceSinks [s] = (:[]) <$> s
sequenceSinks ss = C.sequenceSinks ss

annSamHeaderParser :: Int -> [Annotator] -> CountOpts -> C.Sink ByteLine NGLessIO [Annotator]
annSamHeaderParser mapthreads anns opts = lineGroups =$= sequenceSinks (map annSamHeaderParser1 anns)
    where
        annSamHeaderParser1 (SeqNameAnnotator Nothing) = do
            chunks <- asyncMapEitherC mapthreads (V.mapM seqNameSize)
                =$= CL.fold (flip (:)) []
            vsorted <- liftIO $ do
                v <- V.unsafeThaw (V.concat chunks)
                sortParallel mapthreads v
                V.unsafeFreeze v
            return $! SeqNameAnnotator (Just vsorted)
        annSamHeaderParser1 (GeneMapAnnotator gmap isizes)
            | optNormSize opts = do
                msizes <- liftIO $ V.thaw isizes
                asyncMapEitherC mapthreads (liftM flattenVs . V.mapM (indexUpdates gmap))
                    =$= CL.mapM_ (liftIO . updateSizes msizes)
                GeneMapAnnotator gmap <$> liftIO (V.unsafeFreeze msizes)
        annSamHeaderParser1 ann = C.sinkNull >> return ann
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
        rsiAdd !val rsi@RefSeqInfo{ rsiSize = cur } = rsi { rsiSize = cur + val }

        indexUpdates :: GeneMapAnnotation -> (Int, ByteLine) -> NGLess [(Int, Double)]
        indexUpdates gmap line = do
            RefSeqInfo seqid val <- seqNameSize line
            let ixs = fromMaybe [] $ M.lookup seqid gmap
            return [(ix,val) | ix <- ixs]
        seqNameSize :: (Int, ByteLine) -> NGLess RefSeqInfo
        seqNameSize (n, ByteLine h) = case B8.split '\t' h of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return $! RefSeqInfo (B.drop 3 seqname) (convert size)
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")


listNub :: (Ord a) => [a] -> [a]
listNub = S.toList . S.fromList


-- Takes a vector of [Int] and splits into singletons (which can be represented
-- as `VU.Vector Int` and the rest (represented as `IG.IntGroups`)
splitSingletons :: MMMethod -> V.Vector [Int] -> (VU.Vector Int, IG.IntGroups)
splitSingletons method values = (singles, mms)
    where
        singles = VU.create $ do
            v <- VU.unsafeThaw $ VU.unfoldr getsingle1 0
            -- We want to maximize the work performed in this function as it is
            -- being done in a worker thread:
            -- sorting is completely unnecessary for correctness, but improves
            -- cache performance as close-by indices will be accessed together
            -- when this data is processed in the main thread.
            VA.sort v

            return v
        getsingle1 :: Int -> Maybe (Int, Int)
        getsingle1 ix = do
            vs <- values V.!? ix
            case vs of
                [v] -> return (v, ix + 1)
                _ -> getsingle1 (ix + 1)
        mms -- if we are only using unique hits, then we do not need to care about non-singletons
            | method == MMUniqueOnly = IG.empty
            | otherwise = IG.fromList (filter larger1 (V.toList values))
        larger1 []  = False
        larger1 [_] = False
        larger1 _   = True


performCount :: FilePath -> T.Text -> [Annotator] -> CountOpts -> NGLessIO FilePath
performCount samfp gname annotators0 opts = do
    outputListLno' TraceOutput ["Starting count..."]
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        method = optMMMethod opts
        delim = optDelim opts

    (samcontent, annotators) <-
        samBamConduit samfp
            =$= linesC
            $$+ C.takeWhile ((=='@') . B8.head . unwrapByteLine)
            =$= annSamHeaderParser mapthreads annotators0 opts
    outputListLno' TraceOutput ["Loaded headers. Starting parsing/distribution."]

    mcounts <- forM annotators $ \ann -> do
        let n_entries = annSize ann
        liftIO $ VUM.replicate n_entries (0.0 :: Double)
    toDistribute <-
            samcontent
                $$+- readSamGroupsC' mapthreads
                =$= asyncMapEitherC mapthreads (\samgroup -> forM annotators $ \ann -> do
                                                                annotated <- V.mapM (annotateReadGroup opts ann) samgroup
                                                                return $ splitSingletons method annotated)
                =$= sequenceSinks [CL.map (!! i) =$= performCount1Pass method mc | (i,mc) <- zip [0..] mcounts]
    sizes <- if optNormSize opts || method == MMDist1
                then forM annotators $ \ann -> do
                    let n_entries = annSize ann
                    sizes <- liftIO $ VUM.new n_entries
                    forM_ (annEnumerate ann) $ \(name,i) -> do
                        s <- runNGLess $ annSizeOf ann name
                        liftIO $ VUM.write sizes i s
                    return sizes
                else forM mcounts $ const (liftIO $ VUM.new 0)

    raw_counts <- if method == MMDist1
                    then forM mcounts (liftIO . VUM.clone)
                    else return mcounts
    when (optNormSize opts || method == MMDist1) $
        forM_ (zip mcounts sizes) (uncurry normalizeCounts)
    counts <- forM mcounts (liftIO . VU.unsafeFreeze)

    results <-
        if method /= MMDist1
            then return counts
            else do
                outputListLno' TraceOutput ["Counts (second pass)..."]
                forM (zip4 counts raw_counts sizes toDistribute) $ \(c, r, s, t) -> do
                    distributeMM t c r False
                    when (optNormSize opts) $
                        normalizeCounts r s
                    liftIO $ VU.unsafeFreeze r

    (newfp,hout) <- openNGLTempFile samfp "counts." "txt"
    liftIO $ do
        BL.hPut hout (BL.fromChunks [delim, T.encodeUtf8 gname, "\n"])
        forM_ (zip annotators results) $ \(ann,result) -> do
            mheaders <- VM.new (annSize ann)
            forM_ (annEnumerate ann) $ \(v,i) ->
                VM.write mheaders i v
            headers <- V.unsafeFreeze mheaders

            let nlB :: BB.Builder
                nlB = BB.word8 10
                tabB :: BB.Builder
                tabB = BB.word8 9
                encode1 :: Int -> BB.Builder
                encode1 !i = let
                            hn = (V.!) headers i
                            v = (VU.!) result i
                        in if v >= optMinCount opts
                                    then mconcat [BB.byteString hn, tabB, BB.byteString (toShortest v), nlB]
                                    else mempty

                n = VU.length result
            mapM_ (BB.hPutBuilder hout. encode1) [0 .. n - 1]
        hClose hout
    return newfp

incrementAll :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAll counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

incrementAllV :: VUM.IOVector Double -> [Int] -> IO ()
incrementAllV counts vis = forM_ vis $ \vi -> unsafeIncrement counts vi

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

data LoadFunctionalMapState = LoadFunctionalMapState
                                        !Int -- ^ next free index
                                        !(M.Map B.ByteString [Int]) -- ^ gene -> [feature-ID]
                                        !(M.Map B.ByteString Int) -- ^ feature -> feature-ID

loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO [Annotator]
loadFunctionalMap fname [] = throwScriptError ("Loading annotation file '"++fname++"' but no features requested. This is probably a bug.")
loadFunctionalMap fname columns = do
        outputListLno' InfoOutput ["Loading map file ", fname]
        (resume, [headers]) <- (CB.sourceFile fname =$ CB.lines =$= enumerate)
                $$+ (CL.isolate 1 =$= CL.map (B8.split '\t' . snd) =$ CL.consume)
        cis <- runNGLess $ lookUpColumns headers
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        anns <- map finishFunctionalMap <$> (resume
                $$+- C.conduitVector 8192
                =$= asyncMapEitherC mapthreads (V.mapM (selectColumns cis)) -- after this we have vectors of (<gene name>, [<feature-name>])
                =$ sequenceSinks
                    [CL.fold (V.foldl' (inserts1 c)) (LoadFunctionalMapState 0 M.empty M.empty) | c <- [0 .. length cis - 1]])
        outputListLno' TraceOutput ["Loading of map file '", fname, "' complete"]
        return anns
    where
        finishFunctionalMap (LoadFunctionalMapState _ gmap namemap) = GeneMapAnnotator
                                                                            (reindex gmap namemap)
                                                                            (V.fromList [RefSeqInfo n 0.0 | n <- M.keys namemap])
        reindex :: M.Map B.ByteString [Int] -> M.Map B.ByteString Int -> M.Map B.ByteString [Int]
        reindex gmap namemap = M.map (map reindex1) gmap
            where
                reindex1 :: Int -> Int
                reindex1 = fromJust . flip M.lookup remap
                remap = M.fromList (zip (M.elems namemap) [0..])

        inserts1 :: Int -> LoadFunctionalMapState -> (B.ByteString, [[B.ByteString]]) -> LoadFunctionalMapState
        inserts1 c (LoadFunctionalMapState first gmap namemap) (name, ids) = LoadFunctionalMapState first' gmap' namemap'
            where
                (first', namemap', ids') = foldl' insertname (first,namemap,[]) (ids !! c)
                gmap' = M.insert name ids' gmap

                insertname :: (Int, M.Map B.ByteString Int, [Int]) -> B.ByteString -> (Int, M.Map B.ByteString Int, [Int])
                insertname (!next, !curmap, ns') n = case M.lookup n curmap of
                    Just ix -> (next, curmap, ix:ns')
                    Nothing -> (next + 1, M.insert n next curmap, next:ns')


        lookUpColumns :: [B.ByteString] -> NGLess [Int]
        lookUpColumns [] = throwDataError ("Loading functional map file '" ++ fname ++ "': Header line missing!")
        lookUpColumns headers = sort <$> mapM (lookUpColumns' $ M.fromList (zip (tail headers) [0..])) columns
        lookUpColumns' :: M.Map B8.ByteString Int -> B8.ByteString -> NGLess Int
        lookUpColumns' colmap col = note notfounderror $ M.lookup col colmap
            where
                notfounderror = NGError DataError errormsg
                errormsg = concat (["Could not find column '", B8.unpack col, "'."]
                                ++ case findSuggestion (T.pack $ B8.unpack col) (map (T.pack . B8.unpack) $ M.keys colmap) of
                                        Just (Suggestion valid reason) -> [" Did you mean '", T.unpack valid, "' (", T.unpack reason, ")?"]
                                        Nothing -> [])
        selectColumns :: [Int] -> (Int, B.ByteString) -> NGLess (B.ByteString, [[B.ByteString]])
        selectColumns cols (line_nr, line) = case B8.split '\t' line of
                    (gene:mapped) -> (gene,) . addTags columns <$> selectIds line_nr cols (zip [0..] mapped)
                    [] -> throwDataError ("Loading functional map file '" ++ fname ++ "' [line " ++ show (line_nr + 1)++ "]: empty line.")

        addTags :: [B.ByteString] -> [B.ByteString] -> [[B.ByteString]]
        addTags [] _ = error "impossible"
        addTags [_] [v] = [B8.split ',' v] -- do not tag single features
        addTags fs vss = [[B.concat [f, ":", v] | v <- B8.split ',' vs]
                                    | (f,vs) <- zip fs vss]

        selectIds :: Int -> [Int] -> [(Int, B.ByteString)] -> NGLess [B.ByteString]
        selectIds _ [] _ = return []
        selectIds line_nr fs@(fi:rest) ((ci,v):vs)
            | fi == ci = (v:) <$> selectIds line_nr rest vs
            | otherwise = selectIds line_nr fs vs
        selectIds line_nr _ _ = throwDataError ("Loading functional map file '" ++ fname ++ "' [line " ++ show (line_nr + 1)++ "]: wrong number of columns") -- humans count lines in 1-based systems


annotationMode :: [B.ByteString] -> Maybe T.Text -> Maybe FilePath -> Maybe FilePath -> NGLessIO AnnotationMode
annotationMode _ _ (Just _) (Just _) = throwScriptError "Cannot simmultaneously pass a gff_file and an annotation_file for count() function"
annotationMode ["seqname"] _ _ _ = return AnnotateSeqName
annotationMode _ _ (Just r) _ = return (AnnotateFunctionalMap r)
annotationMode _ _ _ (Just g) = return (AnnotateGFF g)
annotationMode _ (Just ref) Nothing Nothing = do
    outputListLno' InfoOutput ["Annotate with reference: ", show ref]
    ReferenceFilePaths _ mgffpath mfuncpath <- ensureDataPresent ref
    case (mgffpath, mfuncpath) of
        (Just gffpath, Nothing) -> return $! AnnotateGFF gffpath
        (Nothing, Just fmpath) -> return $! AnnotateFunctionalMap fmpath
        (Nothing, Nothing) -> throwScriptError ("Could not find annotation file for '" ++ T.unpack ref ++ "'")
        (Just _, Just _) -> throwDataError ("Reference " ++ T.unpack ref ++ " has both a GFF and a functional map file. Cannot figure out what to do.")
annotationMode _ _ _ _ =
            throwShouldNotOccur "For counting, you must do one of\n1. use seqname mode\n2. pass in a GFF file using the argument 'gff_file'\n3. pass in a gene map using the argument 'functional_map'"

loadGFF :: FilePath -> CountOpts -> NGLessIO [Annotator]
loadGFF gffFp opts = do
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "'..."]
        partials <- C.runConduit $
                conduitPossiblyCompressedFile gffFp
                =$= CB.lines
                =$= readAnnotationOrDie
                =$= sequenceSinks
                    [CL.fold (insertg f) (0, M.empty, M.empty, M.empty) | f <- optFeatures opts]

        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "' complete."]
        return $! map finishGffAnnotator partials
    where
        singleFeature = length (optFeatures opts) == 1
        readAnnotationOrDie :: C.Conduit B.ByteString NGLessIO GffLine
        readAnnotationOrDie = C.awaitForever $ \line ->
            unless (B8.head line == '#') $
                case readGffLine line of
                    Right g -> C.yield g
                    Left err -> throwError err
        finishGffAnnotator ::  (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double) -> Annotator
        finishGffAnnotator (_, amap,namemap,szmap) = GFFAnnotator amap' headers szmap
            where (amap',headers) = reindex amap namemap
        -- The signature looks hairy, but we pass a tuple to have the state while iterating using the fold.
        --  - next: next available ID
        --  - gmap: current annotation map
        --  - namemap: str -> int name to ID
        --  - szmap: str -> double name to feature size
        insertg :: B.ByteString -> (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double) -> GffLine -> (Int, GFFAnnotationMap, M.Map B.ByteString Int, M.Map B.ByteString Double)
        insertg f cur@(next, gmap, namemap, szmap) gline
                | gffType gline /= f = cur
                | otherwise = (next', gmap', namemap', szmap')
            where
                header
                    | singleFeature = gffId gline
                    | otherwise = B.concat [B8.pack (show $ gffType gline), "\t", gffId gline]
                (namemap', active, !next') = case M.lookup header namemap of
                    Just v -> (namemap, v, next)
                    Nothing -> (M.insert header next namemap, next, next+1)

                gmap' :: GFFAnnotationMap
                gmap' = M.alter insertg' (gffSeqId gline) gmap
                insertg' immap = Just $ IM.alter
                                            (\vs -> Just ((gffStrand gline :!: active):fromMaybe [] vs))
                                            asInterval
                                            (fromMaybe IM.empty immap)

                asInterval :: IM.Interval Int
                asInterval = IM.ClosedInterval (gffStart gline) (gffEnd gline)

                szmap' = M.alter inserts1 header szmap
                inserts1 :: Maybe Double -> Maybe Double
                inserts1 val = Just $! convert (gffSize gline) + fromMaybe 0.0 val
        -- First integer IDs are assigned "first come, first served"
        -- `reindex` makes them alphabetical
        reindex :: GFFAnnotationMap -> M.Map B.ByteString Int -> (GFFAnnotationMap, [B.ByteString])
        reindex amap namemap = (M.map (fmap (map reindexAI)) amap, headers)
            where
                headers = M.keys namemap -- these are sorted
                reindexAI :: AnnotationInfo -> AnnotationInfo
                reindexAI (s :!: v) = (s :!: fromJust (M.lookup v ix2ix))
                ix2ix = M.fromList $ zip (M.elems namemap) [0..]

        gffSize :: GffLine -> Int
        gffSize g = (gffEnd g - gffStart g) + 1 -- gff format is inclusive at both ends!



annotateSamLineGFF :: CountOpts -> GFFAnnotationMap -> SamLine -> [Int]
annotateSamLineGFF opts amap samline = case M.lookup rname amap of
        Nothing -> []
        Just im ->  TU.snd <$> (optIntersectMode opts) im lineStrand (sStart, sEnd)
    where
        rname = samRName samline
        sStart = samPos samline
        sEnd   = sStart + samLength samline - 1
        lineStrand :: GffStrand
        lineStrand = if optStrandSpecific opts
                        then if isPositive samline then GffPosStrand else GffNegStrand
                        else GffUnStranded

filterStrand :: GffStrand -> IM.IntervalMap Int [AnnotationInfo] -> IM.IntervalMap Int [AnnotationInfo]
filterStrand GffUnStranded = id
filterStrand strand = IM.mapMaybe $ \ais -> case filter matchStrand ais of
                                                    [] -> Nothing
                                                    ais' -> Just ais'
    where
        matchStrand (s :!: _) = s == GffUnStranded || s == strand

union :: AnnotationRule
union im strand (sS, sE) =  concat . IM.elems . filterStrand strand . IM.intersecting im $ IM.ClosedInterval sS sE

intersection_strict :: AnnotationRule
intersection_strict im strand (sS, sE) = intersection' $ map (filterStrand strand . IM.containing im) [sS..sE]

intersection_non_empty :: AnnotationRule
intersection_non_empty im strand (sS, sE) = intersection' . filter (not . null) .  map (filterStrand strand . IM.containing subim) $ [sS..sE]
    where
        subim = IM.intersecting im (IM.ClosedInterval sS sE)

intersection' :: [GffIMMap] -> [AnnotationInfo]
intersection' [] = []
intersection' im = concat . IM.elems $ foldl1' IM.intersection im

lookupFilePath context name args = case lookup name args of
    Nothing -> return Nothing
    Just a -> Just <$> stringOrTypeError context a

