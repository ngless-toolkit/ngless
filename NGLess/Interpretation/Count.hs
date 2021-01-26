{- Copyright 2015-2020 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP, TypeApplications #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
module Interpretation.Count
    ( executeCount
    , executeCountCheck
    , Annotator(..)
    , CountOpts(..)
    , AnnotationMode(..)
    , AnnotationIntersectionMode(..)
    , MMMethod(..)
    , NMode(..)
    , StrandMode(..)
    , annotationRule
    , loadAnnotator
    , loadFunctionalMap
    , performCount
    , RSV.RefSeqInfo(..)
#ifdef IS_BUILDING_TEST
    , AnnotationInfo(..)
#endif
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VA

import qualified Data.IntervalIntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Algorithms.Async as CAlg
import qualified Data.Conduit.Algorithms.Utils as CAlg
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import           Data.Conduit ((.|))
import           Data.Strict.Tuple (Pair(..))
import           Control.Monad (when, unless, forM, forM_, foldM)
import           Foreign.Storable (Storable(..))
import           Foreign.Ptr (castPtr)
import           Control.Monad.Primitive (PrimMonad(..))


import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class   (liftIO)
import Data.List                (foldl', sort, sortOn)
import GHC.Conc                 (getNumCapabilities)
import Control.DeepSeq          (NFData(..))
import Control.Error            (note)
import Control.Applicative      ((<|>))
import Data.Maybe

import Data.Convertible         (convert)

import Data.GFF
import Data.Sam (SamLine(..), isSamHeaderString, samLength, isAligned, isPositive, readSamGroupsC')
import FileManagement (makeNGLTempFile, expandPath)
import NGLess.NGLEnvironment
import ReferenceDatabases
import NGLess.NGError
import FileOrStream
import Language
import Output
import NGLess

import Utils.Utils
import Utils.Vector
import Utils.Conduit
import Utils.Suggestion
import qualified Utils.IntGroups as IG
import qualified Interpretation.Count.RefSeqInfoVector as RSV

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
 - annotating them with an Annotator. At a high level this code does:
 -
 - annotators <- loadAnnotators opts
 - readgroups <- loadReadGroups inputSAMFile
 - annotated <- forM annotators $ \ann ->
 -                  forM readgroups $ \rg ->
 -                      annotate ann rg
 - final <- forM annotated (normalize opts)
 -
 - While this is simple, the actual code is much more complex for efficiency
 - and because annotation of read groups has a lot of complicated subcases. One
 - particularly optimization is that the annotations are done by mapping to
 - integer indices. While this is much more error prone than using the strings
 - directly, it proved a massive speed-up in computation and memory usage.
 -
 - There are three annotation modes:
 -
 -   1. seqname
 -   2. GFF-based
 -   3. MOCAT-style "gene name" -> "feature"
 -}

data AnnotationInfo = AnnotationInfo !GffStrand !Int
    deriving (Eq)

instance NFData AnnotationInfo where
    rnf (AnnotationInfo !_ !_) = ()


instance Storable AnnotationInfo where
    sizeOf _ = 2 * sizeOf (undefined :: Int)
    alignment _ = alignment (undefined :: Int)
    poke ptr (AnnotationInfo s ix) =
        let
            ptr' = castPtr ptr
        in do
            pokeElemOff @Int ptr' 0 (fromEnum s)
            pokeElemOff @Int ptr' 1 (fromEnum ix)

    peek ptr =
        let
            ptr' = castPtr ptr
        in do
            s <- peekElemOff @Int ptr' 0
            ix <- peekElemOff @Int ptr' 1
            return $! AnnotationInfo
                    (toEnum s)
                    (toEnum ix)



type GffIMMap = IM.IntervalIntMap AnnotationInfo
type GffIMMapAcc = IM.IntervalIntMapAccumulator (PrimState IO) AnnotationInfo

-- GFFAnnotationMap maps from `References` (e.g., chromosomes) to positions to (strand/feature-id)
type GFFAnnotationMap = M.Map BS.ShortByteString GffIMMap

type GFFAnnotationMapAcc = M.Map BS.ShortByteString GffIMMapAcc
type AnnotationRule = GffIMMap -> GffStrand -> IM.Interval -> [AnnotationInfo]

-- This implements MOCAT-style "gene name" -> "feature" annotation
type GeneMapAnnotation = M.Map B.ByteString [Int]

data MMMethod = MMCountAll | MM1OverN | MMDist1 | MMUniqueOnly
    deriving (Eq)

data NMode = NMRaw | NMNormed | NMScaled | NMFpkm
    deriving (Eq)

data StrandMode = SMBoth | SMSense | SMAntisense
    deriving (Eq)

minDouble :: Double
minDouble = (2.0 :: Double) ^^ fst (floatRange (1.0 :: Double))

data CountOpts =
    CountOpts
    { optFeatures :: [B.ByteString] -- ^ list of features to condider
    , optSubFeatures :: Maybe [B.ByteString] -- ^ list of sub-features to condider
    , optAnnotationMode :: !AnnotationMode
    , optIntersectMode :: AnnotationRule
    , optStrandMode :: !StrandMode
    , optMinCount :: !Double
    , optMMMethod :: !MMMethod
    , optDelim :: !B.ByteString
    , optNormMode :: !NMode
    , optIncludeMinus1 :: !Bool
    }

data AnnotationMode = AnnotateSeqName | AnnotateGFF FilePath | AnnotateFunctionalMap FilePath
    deriving (Eq)

data Annotator =
                SeqNameAnnotator (Maybe RSV.RefSeqInfoVector) -- ^ Just annotate by sequence names
                | GFFAnnotator GFFAnnotationMap (V.Vector BS.ShortByteString) !(VU.Vector Double) -- ^ map reference regions to features + feature sizes
                | GeneMapAnnotator B.ByteString GeneMapAnnotation RSV.RefSeqInfoVector -- ^ map reference (gene names) to indices, indexing into the vector of refseqinfo
instance NFData Annotator where
    rnf (SeqNameAnnotator m) = rnf m
    rnf (GFFAnnotator amap headers szmap) = amap `seq` rnf headers `seq` rnf szmap -- amap is already strict
    rnf (GeneMapAnnotator !_ amap szmap) = rnf amap `seq` rnf szmap

annotateReadGroup :: CountOpts -> Annotator -> [SamLine] -> NGLess [Int]
annotateReadGroup opts ann samlines = add1 . listNub <$> case ann of
        SeqNameAnnotator Nothing -> throwShouldNotOccur "Incomplete annotator used"
        SeqNameAnnotator (Just szmap) -> mapMaybeM (getID szmap) samlines
        GFFAnnotator amap _ _ -> return . concatMap (annotateSamLineGFF opts amap) $ samlines
        GeneMapAnnotator _ amap _ -> return . concatMap (mapAnnotation1 amap) $ samlines
    where
        -- this is because "unmatched" is -1
        add1 [] = [0]
        add1 vs = (+ 1) <$> vs
        getID :: RSV.RefSeqInfoVector -> SamLine -> NGLess (Maybe Int)
        getID szmap sr@SamLine{samRName = rname }
            | isAligned sr = case RSV.lookup szmap rname of
                    Nothing -> throwDataError ("Unknown sequence id: " ++ show rname)
                    ix -> return ix
        getID _ _ = Right Nothing
        mapAnnotation1 :: GeneMapAnnotation ->  SamLine -> [Int]
        mapAnnotation1 amap samline = fromMaybe [] $ M.lookup (samRName samline) amap

annSizeAt :: Annotator -> Int -> NGLess Double
annSizeAt _ 0 = return 0.0
annSizeAt (SeqNameAnnotator Nothing) _ = throwShouldNotOccur "Using unloaded annotator"
annSizeAt ann ix
    | ix >= annSize ann = throwShouldNotOccur "Looking up size of inexistent index counts/annSizeAt"
annSizeAt (SeqNameAnnotator (Just vec)) ix = return $! RSV.retrieveSize vec (ix - 1)
annSizeAt (GFFAnnotator _ _ szmap) ix = return $! szmap VU.! (ix - 1)
annSizeAt (GeneMapAnnotator _ _ vec) ix = return $! RSV.retrieveSize vec (ix - 1)

annEnumerate :: Annotator -> [(B.ByteString, Int)]
annEnumerate (SeqNameAnnotator Nothing)   = error "Using unfinished annotator"
annEnumerate (SeqNameAnnotator (Just ix)) = ("-1",0):enumerateRSVector ix
annEnumerate (GeneMapAnnotator tag _ ix) = let
                            addTag
                                | B.null tag = id
                                | otherwise = \(name, v) -> (B.concat [tag, ":", name], v)
                        in addTag <$> ("-1",0):enumerateRSVector ix
annEnumerate (GFFAnnotator _ headers _)   = zip ("-1":(map BS.fromShort $ V.toList headers)) [0..]
enumerateRSVector rfv = [(RSV.retrieveName rfv i, i + 1) | i <- [0.. RSV.length rfv - 1]]

-- Number of elements
annSize :: Annotator -> Int
annSize (SeqNameAnnotator Nothing) = error "annSize (SeqNameAnnotator Nothing) is illegal"
annSize (SeqNameAnnotator (Just rfv)) = RSV.length rfv + 1
annSize (GeneMapAnnotator _ _ rfv) = RSV.length rfv + 1
annSize (GFFAnnotator _ _ szmap) = VU.length szmap + 1

{- We define the type AnnotationIntersectionMode mainly to facilitate tests,
 - which depend on being able to write code such as
 -
 -      annotationRule IntersectUnion
 -}
data AnnotationIntersectionMode = IntersectUnion | IntersectStrict | IntersectNonEmpty
    deriving (Eq, Show)


annotationRule :: AnnotationIntersectionMode -> AnnotationRule
annotationRule IntersectUnion = union
annotationRule IntersectStrict = intersection_strict
annotationRule IntersectNonEmpty = intersection_non_empty

parseOptions :: Maybe (Maybe T.Text) -> KwArgsValues -> NGLessIO CountOpts
parseOptions mappedref args = do
    when ("strand" `elem` (map fst args) && "sense" `elem` (map fst args)) $
        (case lookup "original_lno" args of
            Just (NGOInteger lno) -> flip outputListLno (Just $ fromIntegral lno)
            _ -> outputListLno') WarningOutput ["Both `strand` and `sense` arguments passed to count() function. The `strand` argument will be ignored.\n"]
    minCount <- lookupIntegerOrScriptErrorDef (return 0) "count argument parsing" "min" args
    method <- decodeSymbolOrError "multiple argument in count() function"
                    [("1overN", MM1OverN)
                    ,("dist1", MMDist1)
                    ,("all1", MMCountAll)
                    ,("unique_only", MMUniqueOnly)
                    ] =<< lookupSymbolOrScriptErrorDef (return "dist1")
                                    "multiple argument to count " "multiple" args
    strand_specific <- lookupBoolOrScriptErrorDef (return False) "count function" "strand" args
    smode <- decodeSymbolOrError "strand argument to count() function"
                    [("both", SMBoth)
                    ,("sense", SMSense)
                    ,("antisense", SMAntisense)
                    ] =<< lookupSymbolOrScriptErrorDef (return (if strand_specific then "sense" else "both")) "count function" "sense" args
    include_minus1 <- lookupBoolOrScriptErrorDef defaultMinus1 "count function" "include_minus1" args
    mocatMap <- lookupFilePath "functional_map argument to count()" "functional_map" args
    gffFile <- lookupFilePath "gff_file argument to count()" "gff_file" args
    discardZeros <- lookupBoolOrScriptErrorDef (return False) "count argument parsing" "discard_zeros" args
    m <- fmap annotationRule $ decodeSymbolOrError "mode argument to count"
                    [("union", IntersectUnion)
                    ,("intersection_strict", IntersectStrict)
                    ,("intersection_non_empty", IntersectNonEmpty)
                    ] =<< lookupSymbolOrScriptErrorDef (return "union") "mode argument to count" "mode" args
    delim <- T.encodeUtf8 <$> lookupStringOrScriptErrorDef (return "\t") "count hidden argument (should always be valid)" "__delim" args
    when ("norm" `elem` (fst <$> args) && "normalization" `elem` (fst <$> args)) $
        outputListLno' WarningOutput ["In count() function: both `norm` and `normalization` used. `norm` is semi-deprecated and will be ignored in favor of `normalization`"]
    normSize <- lookupBoolOrScriptErrorDef (return False) "count function" "norm" args
    normMode <- decodeSymbolOrError "normalization option"
                        [("raw", NMRaw)
                        ,("normed", NMNormed)
                        ,("scaled", NMScaled)
                        ,("fpkm", NMFpkm)] =<< lookupSymbolOrScriptErrorDef
                                                    (return $! if normSize then "normed" else "raw") "count function" "normalization" args
    fs <- case lookup "features" args of
        Nothing -> return ["gene"]
        Just (NGOString f) -> return [f]
        Just (NGOList feats') -> mapM (stringOrTypeError "count features argument") feats'
        _ -> throwShouldNotOccur "executeAnnotation: TYPE ERROR"
    subfeatures <- case lookup "subfeatures" args of
        Nothing -> return Nothing
        Just (NGOString sf) -> return $ Just [sf]
        Just (NGOList subfeats') -> Just <$> mapM (stringOrTypeError "count subfeatures argument") subfeats'
        _ -> throwShouldNotOccur "executeAnnotation: TYPE ERROR"
    refinfo <- case lookup "reference" args of
        Nothing -> return mappedref
        Just val -> Just . Just <$> stringOrTypeError "reference for count()" val
    let features = map (B8.pack . T.unpack) fs
        parseAnnotationMode :: [B.ByteString] -> Maybe (Maybe T.Text) -> Maybe FilePath -> Maybe FilePath -> NGLessIO AnnotationMode
        parseAnnotationMode _ _ (Just _) (Just _) =
            throwScriptError "Cannot simultaneously pass a gff_file and an annotation_file for count() function"
        parseAnnotationMode ["seqname"] _ _ _ = return AnnotateSeqName
        parseAnnotationMode _ _ (Just r) _ = return (AnnotateFunctionalMap r)
        parseAnnotationMode _ _ _ (Just g) = return (AnnotateGFF g)
        parseAnnotationMode _ (Just (Just ref)) Nothing Nothing = do
            outputListLno' InfoOutput ["Annotate with reference: ", show ref]
            ReferenceFilePaths _ mgffpath mfuncpath <- ensureDataPresent ref
            case (mgffpath, mfuncpath) of
                (Just gffpath, Nothing) -> return $! AnnotateGFF gffpath
                (Nothing, Just fmpath) -> return $! AnnotateFunctionalMap fmpath
                (Nothing, Nothing) -> throwScriptError ("Could not find annotation file for '" ++ T.unpack ref ++ "'")
                (Just _, Just _) -> throwDataError ("Reference " ++ T.unpack ref ++ " has both a GFF and a functional map file. Cannot figure out what to do.")
        parseAnnotationMode _ Nothing _ _ = return AnnotateSeqName -- placeholder, but will only happen in __check_count call
        parseAnnotationMode _ _ _ _ =
            throwScriptError ("For counting, you must do one of\n" ++
                              "1. use seqname mode\n" ++
                              "2. pass in a GFF file using the argument 'gff_file'\n" ++
                              "3. pass in a gene map using the argument 'functional_map'")


    amode <- parseAnnotationMode features refinfo mocatMap gffFile
    return $! CountOpts
            { optFeatures = features
            , optSubFeatures = map (B8.pack . T.unpack) <$> subfeatures
            , optAnnotationMode = amode
            , optIntersectMode = m
            , optStrandMode = smode
            , optMinCount = if discardZeros
                                then minDouble
                                else fromInteger minCount
            , optMMMethod = method
            , optDelim = delim
            , optNormMode = normMode
            , optIncludeMinus1 = include_minus1
            }

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOMappedReadSet rname istream mappedref) args = do
    opts <- parseOptions (Just mappedref) args
    annotators <- loadAnnotator (optAnnotationMode opts) opts
    NGOCounts . File <$> performCount istream rname annotators opts
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)

executeCountCheck :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCountCheck _ kwargs = do
    opts <- parseOptions Nothing kwargs
    lno <- lookupIntegerOrScriptErrorDef (return 0) "hidden lno argument" "original_lno" kwargs
    case optAnnotationMode opts of
        AnnotateFunctionalMap fname -> do
            columns <- C.runConduit $
                    conduitPossiblyCompressedFile fname
                    .| linesC
                    .| CAlg.enumerateC
                    .| (lastCommentOrHeader fname True >>= \case
                            Nothing -> return []
                            Just (_,ByteLine line) -> return $ B8.split '\t' line)
            let missing = [f | f <- optFeatures opts, f `notElem` columns]
            case missing of
                [] -> return ()
                ms -> do
                    let errormsg = [
                                "In call to count() [line ",
                                show lno,
                                "], missing features:"
                                ] ++ concat [[" ", B8.unpack f]  | f <- ms]
                    throwDataError (concat errormsg)
        _ -> return ()
    return NGOVoid

-- | The include_minus1 argument defaulted to False up to version 0.5. Now, it
-- defaults to true as it seems to be what most users expect.
defaultMinus1 :: NGLessIO Bool
defaultMinus1 = do
    v <- ngleVersion <$> nglEnvironment
    return $! v > NGLVersion 0 5

loadAnnotator :: AnnotationMode -> CountOpts -> NGLessIO [Annotator]
loadAnnotator AnnotateSeqName _ = return [SeqNameAnnotator Nothing]
loadAnnotator (AnnotateGFF gf) opts = loadGFF gf opts
loadAnnotator (AnnotateFunctionalMap mm) opts = loadFunctionalMap mm (optFeatures opts)


-- First pass over the data
performCount1Pass :: MMMethod
                        -> VUM.IOVector Double -- ^ counts vector. Will be modified
                        -> C.ConduitT (VU.Vector Int, IG.IntGroups) C.Void NGLessIO [IG.IntGroups]
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
        loop :: [IG.IntGroups] -> C.ConduitT (VU.Vector Int, IG.IntGroups) C.Void NGLessIO [IG.IntGroups]
        loop acc = C.await >>= \case
            Nothing -> return acc
            Just (singles, mms) ->  do
                    liftIO $ incrementAll mcounts singles
                    loop $ if not (IG.null mms)
                                then mms:acc
                                else acc

-- | This is a version of C.sequenceSinks which optimizes the case where a
-- single element is passed (it makes a small, but noticeable difference in
-- benchmarking)
sequenceSinks :: (Monad m) => [C.ConduitT a C.Void m b] -> C.ConduitT a C.Void m [b]
sequenceSinks [s] = (:[]) <$> s
sequenceSinks ss = C.sequenceSinks ss

annSamHeaderParser :: Int -> [Annotator] -> CountOpts -> C.ConduitT ByteLine C.Void NGLessIO [Annotator]
annSamHeaderParser mapthreads anns opts = lineGroups .| sequenceSinks (map annSamHeaderParser1 anns)
    where
        annSamHeaderParser1 (SeqNameAnnotator Nothing) = do
            rfvm <- liftIO RSV.newRefSeqInfoVector
            CAlg.asyncMapEitherC mapthreads (\(!vi, v) -> V.imapM (\ix ell -> seqNameSize (vi*32768+ix, ell)) v)
                .| CL.mapM_ (\v -> liftIO $
                                    V.forM_ v $ \(RSV.RefSeqInfo n val) ->
                                        RSV.insert rfvm n val)
            vsorted <- liftIO $ do
                RSV.sort rfvm
                RSV.unsafeFreeze rfvm
            return $! SeqNameAnnotator (Just vsorted)
        annSamHeaderParser1 (GeneMapAnnotator tag gmap isizes)
            | optNormMode opts == NMNormed = do
                msizes <- liftIO $ RSV.unsafeThaw isizes
                CAlg.asyncMapEitherC mapthreads (\(!vi,headers) -> flattenVs <$> V.imapM (\ix ell -> indexUpdates gmap (vi*32768+ix, ell)) headers)
                    .| CL.mapM_ (liftIO . updateSizes msizes)
                GeneMapAnnotator tag gmap <$> liftIO (RSV.unsafeFreeze msizes)
        annSamHeaderParser1 ann = CC.sinkNull >> return ann
        lineGroups = CL.filter (B.isPrefixOf "@SQ\tSN:" . unwrapByteLine)
                    .| CC.conduitVector 32768
                    .| CAlg.enumerateC
        flattenVs :: VU.Unbox a => V.Vector [a] -> VU.Vector a
        flattenVs chunks = VU.unfoldr getNext (0,[])
            where
                getNext (!vi, v:vs) = Just (v, (vi,vs))
                getNext (vi,[])
                    | vi >= V.length chunks = Nothing
                    | otherwise = getNext (vi + 1, chunks V.! vi)

        updateSizes :: RSV.RefSeqInfoVectorMutable -> VU.Vector (Int,Double) -> IO ()
        updateSizes msizes updates =
            VU.forM_ updates $ \(ix,val) -> do
                cur <- RSV.retrieveSizeIO msizes ix
                RSV.writeSizeIO msizes ix (cur + val)

        indexUpdates :: GeneMapAnnotation -> (Int, ByteLine) -> NGLess [(Int, Double)]
        indexUpdates gmap line = do
            RSV.RefSeqInfo seqid val <- seqNameSize line
            let ixs = fromMaybe [] $ M.lookup seqid gmap
            return [(ix,val) | ix <- ixs]
        seqNameSize :: (Int, ByteLine) -> NGLess RSV.RefSeqInfo
        seqNameSize (n, ByteLine h) = case B8.split '\t' h of
                [_,seqname,sizestr] -> case B8.readInt (B.drop 3 sizestr) of
                    Just (size, _) -> return $! RSV.RefSeqInfo (B.drop 3 seqname) (convert size)
                    Nothing -> throwDataError ("Could not parse sequence length in header (line: " ++ show n ++ ")")
                _ -> throwDataError ("SAM file does not contain the right number of tokens (line: " ++ show n ++ ")")


listNub :: (Ord a) => [a] -> [a]
listNub [] = []
listNub x@[_] = x
listNub x@[a,b]
    | a == b = [a]
    | otherwise = x
listNub other = S.toList . S.fromList $ other


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


performCount :: FileOrStream -> T.Text -> [Annotator] -> CountOpts -> NGLessIO FilePath
performCount istream gname annotators0 opts = do
    outputListLno' TraceOutput ["Starting count..."]
    numCapabilities <- liftIO getNumCapabilities
    let mapthreads = max 1 (numCapabilities - 1)
        method = optMMMethod opts
        delim = optDelim opts
        (samfp, samStream) = asSamStream istream
    (toDistribute, mcounts, annotators) <- C.runConduit $
        samStream
            .| do
                annotators <-
                    CC.takeWhileE (isSamHeaderString . unwrapByteLine)
                        .| CC.concat
                        .| annSamHeaderParser mapthreads annotators0 opts
                lift $ outputListLno' TraceOutput ["Loaded headers. Starting parsing/distribution."]
                mcounts <- forM annotators $ \ann -> do
                    let n_entries = annSize ann
                    liftIO $ VUM.replicate n_entries (0.0 :: Double)
                toDistribute <-
                    readSamGroupsC' mapthreads True
                        .| CAlg.asyncMapEitherC mapthreads (\samgroup -> forM annotators $ \ann -> do
                                                                    annotated <- V.mapM (annotateReadGroup opts ann) samgroup
                                                                    return $ splitSingletons method annotated)
                        .| sequenceSinks [CL.map (!! i) .| performCount1Pass method mc | (i,mc) <- zip [0..] mcounts]
                return (toDistribute, mcounts, annotators)

    results <- distributeScaleCounts (optNormMode opts) (optMMMethod opts) annotators mcounts toDistribute
    makeNGLTempFile samfp "counts." "txt" $ \hout -> liftIO $ do
        BL.hPut hout (BL.fromChunks [delim, T.encodeUtf8 gname, "\n"])
        let maybeSkipM1
                | optIncludeMinus1 opts = id
                | otherwise = tail
        forM_ (zip annotators results) $ \(ann,result) ->
            forM_ (maybeSkipM1 $ annEnumerate ann) $ \(h,i) -> do
                let nlB :: BB.Builder
                    nlB = BB.word8 10
                    tabB :: BB.Builder
                    tabB = BB.word8 9
                    v = (VU.!) result i
                when (v >= optMinCount opts) $
                    BB.hPutBuilder hout $ mconcat [BB.byteString h, tabB, BB.byteString (toShortest v), nlB]


distributeScaleCounts :: NMode -> MMMethod -> [Annotator] -> [VUM.IOVector Double] -> [[IG.IntGroups]] -> NGLessIO [VU.Vector Double]
distributeScaleCounts NMRaw mmmethod _ counts _
    | mmmethod /= MMDist1 = liftIO $ mapM VU.unsafeFreeze counts
distributeScaleCounts norm mmmethod annotators mcountss toDistribute =
    forM (zip3 annotators mcountss toDistribute) $ \(ann, mcounts, indices) -> do
        let n_entries = annSize ann
        sizes <- liftIO $ VUM.new n_entries
        forM_ [0 .. n_entries - 1] $ \i -> do
            s <- runNGLess $ annSizeAt ann i
            liftIO $ VUM.write sizes i s
        redistribute mmmethod mcounts sizes indices
        normalizeCounts norm mcounts sizes
        liftIO $ VU.unsafeFreeze mcounts


-- redistributes the multiple mappers
redistribute :: MMMethod -> VUM.IOVector Double -> VUM.IOVector Double -> [IG.IntGroups] -> NGLessIO ()
redistribute MMDist1 ocounts sizes indices = do
    outputListLno' TraceOutput ["Counts (second pass)..."]
    fractCounts' <- liftIO $ VUM.clone ocounts
    normalizeCounts NMNormed fractCounts' sizes
    fractCounts <- liftIO $ VU.unsafeFreeze fractCounts'
    forM_ indices $ \vss -> IG.forM_ vss $ \vs -> do
        let cs = VU.map (VU.unsafeIndex fractCounts) vs
            cs_sum = sum (VU.toList cs)
            n_cs = convert (VU.length cs)
            adjust :: Double -> Double
            adjust = if cs_sum > 0.0
                        then (/ cs_sum)
                        else const  (1.0 / n_cs)
        forM_ (zip (VU.toList vs) (VU.toList cs)) $ \(v,c) ->
            liftIO $ unsafeIncrement' ocounts v (adjust c)
redistribute _ _ _ _ = return ()

incrementAll :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAll counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

incrementAllV :: VUM.IOVector Double -> VU.Vector Int -> IO ()
incrementAllV counts vis = VU.forM_ vis $ \vi -> unsafeIncrement counts vi

increment1OverN :: VUM.IOVector Double -> VU.Vector Int -> IO ()
increment1OverN counts vis = VU.forM_ vis $ \vi -> unsafeIncrement' counts vi oneOverN
    where
        oneOverN :: Double
        oneOverN = 1.0 / convert (VU.length vis)

normalizeCounts :: NMode -> VUM.IOVector Double -> VUM.IOVector Double -> NGLessIO ()
normalizeCounts NMRaw _ _ = return ()
normalizeCounts NMNormed counts sizes = do
        let n = VUM.length counts
            n' = VUM.length sizes
        unless (n == n') $
            throwShouldNotOccur ("Counts vector is of size " ++ show n ++ ", but sizes is of size " ++ show n')
        liftIO $ forM_ [0 .. n - 1] $ \i -> do
            s <- VUM.read sizes i
            when (s > 0) $
                VUM.unsafeModify counts (/ s) i
normalizeCounts nmethod counts sizes
    | nmethod `elem` [NMScaled, NMFpkm] = do
        -- count vectors always include a -1 at this point (it is
        -- ignored in output if the user does not request it, but is
        -- always computed). Thus, we compute the sum without it and do
        -- not normalize it later:
        let totalCounts v = withVector v (VU.sum . VU.tail)
        initial <- totalCounts counts
        normalizeCounts NMNormed counts sizes
        afternorm <- totalCounts counts
        let factor
                | nmethod == NMScaled = initial / afternorm
                | otherwise = 1.0e9 / initial --- 1e6 [million fragments] * 1e3 [kilo basepairs] = 1e9
        liftIO $ forM_ [1.. VUM.length counts - 1] (VUM.unsafeModify counts (* factor))
    | otherwise = error "This should be unreachable code [normalizeCounts]"



-- lastCommentOrHeader :: Monad m => C.ConduitT (Int, ByteLine) () m (Maybe (Int, ByteLine))
lastCommentOrHeader fname newAPI = C.await >>= \case
                    Nothing -> return Nothing
                    Just f@(_,ByteLine line) ->
                        if isComment line
                            then lastCommentOrHeader' (Just f)
                            else return (Just f)
    where
        lastCommentOrHeader' prev = C.await >>= \case
                        Nothing -> return prev
                        Just f@(_, ByteLine line)
                            | isComment line ->
                                if newAPI
                                    then lastCommentOrHeader' (Just f)
                                    else do
                                        lift $ outputListLno' WarningOutput versionChangeWarning
                                        C.leftover f
                                        return (Just f)
                            | otherwise -> do
                                C.leftover f
                                return prev
        isComment line
            | B.null line = True
            | otherwise = B8.head line == '#'
        versionChangeWarning =
            ["Loading '", fname, "': found several lines at the top starting with '#'.\n",
             "The interpretation of these changed in NGLess 1.1 (they are now considered comment lines).\n",
             "Using the older version for backwards compatibility.\n"]

{- This object keeps the state for iterating over the lines in the annotation
 - file.
 -}
data LoadFunctionalMapState = LoadFunctionalMapState
                                        !Int -- ^ next free index
                                        !(M.Map B.ByteString [Int]) -- ^ gene -> [feature-ID]
                                        !(M.Map B.ByteString Int) -- ^ feature -> feature-ID

-- Loads MOCAT-style TSV files
loadFunctionalMap :: FilePath -> [B.ByteString] -> NGLessIO [Annotator]
loadFunctionalMap fname [] = throwScriptError ("Loading annotation file '"++fname++"' but no features requested. This is probably a bug.")
loadFunctionalMap fname columns = do
        -- There are some complications related to sorting the columns indices.
        -- The returned annotators (if more than one) must be ordered by tag so
        -- that we can output correctly sorted TSV files. While loading the
        -- data, however, the code uses the column order in the file for
        -- extracting the columns,
        outputListLno' InfoOutput ["Loading map file ", fname]
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        v <- ngleVersion <$> nglEnvironment
        anns <- C.runConduit $
                    conduitPossiblyCompressedFile fname
                    .| linesC
                    .| CAlg.enumerateC
                    .| (do
                        hline <- lastCommentOrHeader fname (v >= NGLVersion 1 1)
                        (cis,tags) <- case hline of
                            Nothing -> throwDataError ("Empty map file: "++fname)
                            Just (!line_nr, ByteLine header) -> let headers = B8.split '\t' header
                                                    in runNGLess $ lookUpColumns line_nr headers
                        CC.conduitVector 8192
                            .| CAlg.asyncMapEitherC mapthreads (V.mapM (selectColumns cis)) -- after this we have vectors of (<gene name>, [<feature-name>])
                            .| sequenceSinks
                                [finishFunctionalMap (getTag tags c) <$> CL.fold (V.foldl' (inserts1 c)) (LoadFunctionalMapState 0 M.empty M.empty)
                                        | c <- [0 .. length cis - 1]])
        outputListLno' TraceOutput ["Loading of map file '", fname, "' complete"]
        return $! sortOn (\(GeneMapAnnotator tag _ _) -> tag) anns
    where

        finishFunctionalMap :: B.ByteString -> LoadFunctionalMapState -> Annotator
        finishFunctionalMap tag (LoadFunctionalMapState _ gmap namemap) = GeneMapAnnotator
                                                                            tag
                                                                            (reindex gmap namemap)
                                                                            (RSV.fromList [RSV.RefSeqInfo n 0.0 | n <- M.keys namemap])
        reindex :: M.Map B.ByteString [Int] -> M.Map B.ByteString Int -> M.Map B.ByteString [Int]
        reindex gmap namemap = M.map (map (ix2ix VU.!)) gmap
            where
                ix2ix = revnamemap namemap
        inserts1 :: Int -> LoadFunctionalMapState -> (B.ByteString, [[B.ByteString]]) -> LoadFunctionalMapState
        inserts1 c (LoadFunctionalMapState first gmap namemap) (name, ids) = LoadFunctionalMapState first' gmap' namemap'
            where
                (first', namemap', ids') = foldl' insertname (first,namemap,[]) (ids !! c)
                gmap' = M.insert name ids' gmap

                insertname :: (Int, M.Map B.ByteString Int, [Int]) -> B.ByteString -> (Int, M.Map B.ByteString Int, [Int])
                insertname (!next, !curmap, ns') n = case M.lookup n curmap of
                    Just ix -> (next, curmap, ix:ns')
                    Nothing -> (next + 1, M.insert n next curmap, next:ns')


        lookUpColumns :: Int -> [B.ByteString] -> NGLess ([Int], [B.ByteString])
        lookUpColumns line_nr [] = throwDataError ("Loading functional map file '" ++ fname ++ "' (line " ++ show line_nr ++ "): Header line missing!")
        lookUpColumns _ headers = do
            cis <- mapM (lookUpColumns' $ M.fromList (zip (tail headers) [0..])) columns
            return $ unzip $ sort $ zip cis columns

        lookUpColumns' :: M.Map B.ByteString Int -> B.ByteString -> NGLess Int
        lookUpColumns' colmap col = note notfounderror $ M.lookup col colmap
            where
                notfounderror = NGError DataError errormsg
                errormsg = concat (["Could not find column '", B8.unpack col, "'."]
                                ++ case findSuggestion (T.pack $ B8.unpack col) (map (T.pack . B8.unpack) $ M.keys colmap) of
                                        Just (Suggestion valid reason) -> [" Did you mean '", T.unpack valid, "' (", T.unpack reason, ")?"]
                                        Nothing -> []
                                ++ ["\nAvailable columns are:\n"]
                                ++ ["\t- '"++B8.unpack c ++ "'\n" | c <- M.keys colmap]
                                )
        selectColumns :: [Int] -> (Int, ByteLine) -> NGLess (B.ByteString, [[B.ByteString]])
        selectColumns cols (line_nr, ByteLine line) = case B8.split '\t' line of
                    (gene:mapped) -> (gene,) . splitLines <$> selectIds line_nr cols (zip [0..] mapped)
                    [] -> throwDataError ("Loading functional map file '" ++ fname ++ "' [line " ++ show (line_nr + 1)++ "]: empty line.")

        getTag :: [B.ByteString] -> Int -> B.ByteString
        getTag [_] _ = B.empty
        getTag bs ix = bs !! ix

        splitLines vss = [B8.splitWith (\c -> c ==',' || c == '|') vs | vs <- vss]

        selectIds :: Int -> [Int] -> [(Int, B.ByteString)] -> NGLess [B.ByteString]
        selectIds _ [] _ = return []
        selectIds line_nr fs@(fi:rest) ((ci,v):vs)
            | fi == ci = (v:) <$> selectIds line_nr rest vs
            | otherwise = selectIds line_nr fs vs
        selectIds line_nr _ _ = throwDataError ("Loading functional map file '" ++ fname ++ "' [line " ++ show (line_nr + 1)++ "]: wrong number of columns") -- humans count lines in 1-based systems


revnamemap :: Ord a => M.Map a Int -> VU.Vector Int
revnamemap namemap = VU.create $ do
                r <- VUM.new (M.size namemap)
                forM_ (zip (M.elems namemap) [0..]) $ uncurry (VUM.write r)
                return r

data IntDoublePair = IntDoublePair {-# UNPACK #-} !Int {-# UNPACK #-} !Double
data GffLoadingState = GffLoadingState
                        !GFFAnnotationMapAcc
                        --  ^ gmap: current annotation map
                        !(M.Map BS.ShortByteString IntDoublePair)
                        --  ^ metamap: str -> int name to ID/feature-size

loadGFF :: FilePath -> CountOpts -> NGLessIO [Annotator]
loadGFF gffFp opts = do
        v <- ngleVersion <$> nglEnvironment
        when (not singleFeature && v <= NGLVersion 0 11) $
            throwScriptError (
                "The handling of multiple features/subfeatures has changed in version 1.0\n" ++
                "and we can no longer reproduce the behaviour of NGLess 0.11 and previous\n" ++
                "versions.\n\n"++
                "Please update the version declaration at the top of the NGLess script to\n" ++
                "get the new output format which uses colons (:) to separate the feature\n" ++
                "names (while the old format was a multi-column format).\n\n" ++
                "The old format created problems when mixed with collect() and other\n" ++
                "functions.")
        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "'..."]
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
        partials <- C.runConduit $
                conduitPossiblyCompressedFile gffFp
                    .| linesVC 8192
                    .| CAlg.asyncMapEitherC mapthreads (V.mapM (readGffLine . unwrapByteLine) . V.filter (not . isComment))
                    .| sequenceSinks
                        [CL.foldM (insertgV f sf) (GffLoadingState M.empty M.empty)
                                    |  f <- optFeatures    opts
                                    , sf <- case optSubFeatures opts of
                                                Nothing -> [Nothing]
                                                Just fs -> Just <$> fs]

        outputListLno' TraceOutput ["Loading GFF file '", gffFp, "' complete."]
        mapM finishGffAnnotator partials
    where
        singleFeature
            | length (optFeatures opts) > 1 = False
            | otherwise = case optSubFeatures opts of
                Nothing -> True
                Just [_] -> True
                _ -> False

        isComment (ByteLine line)
            | B.null line = True
            | otherwise = B8.head line == '#'

        insertgV f sf p vs = liftIO $ V.foldM (insertg f sf) p (V.filter ((==f) . gffType) vs)

        -- update GffLoadingState
        insertgV :: B.ByteString -- ^ feature
                        -> Maybe B.ByteString -- ^ subfeature
                        -> GffLoadingState
                        -> V.Vector GffLine
                        -> NGLessIO GffLoadingState
        insertg f sf (GffLoadingState gmap metamap0) gline = do
                    let seqid = BS.toShort $ gffSeqId gline
                    -- We can do it all with a single call to M.alterF, but the
                    -- expectation is that most of the lookups will return
                    -- something and we can avoid allocations
                    (gmap', immap) <- case M.lookup seqid gmap of
                        Just im -> return (gmap, im)
                        Nothing -> do
                            im <- IM.new
                            return (M.insert seqid im gmap, im)
                    let i = IM.Interval (gffStart gline) (gffEnd gline + 1) -- [closed, open) intervals
                    metamap' <- foldM (subfeatureInsert immap i) metamap0 $ lookupSubFeature sf
                    return $! GffLoadingState gmap' metamap'
            where
                subfeatureInsert :: GffIMMapAcc -> IM.Interval -> M.Map BS.ShortByteString IntDoublePair -> B.ByteString -> IO (M.Map BS.ShortByteString IntDoublePair)
                subfeatureInsert !immap !i !metamap sfVal = let
                            header = BS.toShort $ if singleFeature
                                                    then sfVal
                                                    else B.concat $ [f, ":"] ++(case sf of { Nothing -> []; Just s -> [s,":"]}) ++ [sfVal]
                            featureSize :: Double
                            featureSize = convert $ gffSize gline
                            (!metamap', active) = let
                                    combine _key _nv (IntDoublePair p oldSize) = (IntDoublePair p $ oldSize + featureSize)
                                    (oldVal, m) = M.insertLookupWithKey combine header (IntDoublePair (M.size metamap) featureSize) metamap
                                in (m, case oldVal of
                                    Just (IntDoublePair ix _) -> ix
                                    Nothing -> M.size m - 1)
                        in do
                            IM.insert i (AnnotationInfo (gffStrand gline) active) immap
                            return metamap'

                lookupSubFeature :: Maybe B.ByteString -> [B.ByteString]
                lookupSubFeature Nothing = filterSubFeatures "ID" (gffAttrs gline) <|> filterSubFeatures "gene_id" (gffAttrs gline)
                lookupSubFeature (Just s) = filterSubFeatures s (gffAttrs gline)

                filterSubFeatures s sf' = map snd $ filter ((s ==) . fst) sf'

        finishGffAnnotator ::  GffLoadingState -> NGLessIO Annotator
        finishGffAnnotator (GffLoadingState amap metamap) = do
                amap' :!: headers <- reindexGffAnn amap metamap
                let szmap' = VU.fromList $ map (\(IntDoublePair _ v) -> v) $ M.elems metamap
                return $! GFFAnnotator amap' headers szmap'

        -- First integer IDs are assigned "first come, first served"
        -- `reindexGffAnn` makes them alphabetical
        reindexGffAnn :: GFFAnnotationMapAcc -> M.Map BS.ShortByteString IntDoublePair -> NGLessIO (Pair GFFAnnotationMap (V.Vector BS.ShortByteString))
        reindexGffAnn amap metamap = do
            outputListLno' TraceOutput ["Re-index GFF"]
            let headers = V.fromList $ M.keys metamap -- these are sorted
                ix2ix :: VU.Vector Int
                ix2ix = VU.create $ do
                                r <- VUM.new (M.size metamap)
                                forM_ (zip (M.elems metamap) [0..]) $ \(IntDoublePair i _, p) -> VUM.write r i p
                                return r
                reindexAI :: AnnotationInfo -> AnnotationInfo
                reindexAI (AnnotationInfo s v) = AnnotationInfo s (ix2ix VU.! v)
            amap' <- forM amap $ \im -> do
                im' <- IM.unsafeFreeze im
                return $ IM.map reindexAI im'
            return $! amap' :!: headers
        gffSize :: GffLine -> Int
        gffSize g = (gffEnd g - gffStart g) + 1 -- gff format is inclusive at both ends!



-- annotateSamLineGFF: Annotate a SamLine with the annotation map taking into
-- account the relevant options:
--    - the intersection rules
--    - the strandness rules
annotateSamLineGFF :: CountOpts -> GFFAnnotationMap -> SamLine -> [Int]
annotateSamLineGFF opts amap samline = case M.lookup (BS.toShort rname) amap of
        Nothing -> []
        Just im ->  selectIx $ (optIntersectMode opts) im lineStrand (IM.Interval sStart sEnd)
    where
        selectIx = map (\(AnnotationInfo _ ix) -> ix)
        rname = samRName samline
        sStart = samPos samline
        sEnd   = sStart + samLength samline
        -- GffUnStranded matches everything (see 'filterStrand')
        lineStrand :: GffStrand
        lineStrand = case optStrandMode opts of
                            SMBoth -> GffUnStranded
                            SMSense
                                | isPositive samline -> GffPosStrand
                                | otherwise -> GffNegStrand
                            SMAntisense -- reverse strandness
                                | isPositive samline -> GffNegStrand
                                | otherwise -> GffPosStrand

matchStrand GffUnStranded _ = True
matchStrand s (AnnotationInfo s' _) = s == s'

union :: AnnotationRule
union im strand i =  filter (matchStrand strand) . IM.overlaps i $ im

intersection_strict :: AnnotationRule
intersection_strict im strand i = let
        candidates = IM.overlapsWithKeys i  im
        strandFiltered = filter (matchStrand strand . snd) candidates
        intersecting = filter (contained i . fst) strandFiltered
        contained (IM.Interval s0 e0) (IM.Interval s1 e1) = s0 >= s1 && e0 <= e1
    in noDupAIs (fmap snd intersecting)

noDupAIs :: [AnnotationInfo] -> [AnnotationInfo]
noDupAIs = noDupAIs' []
    where
        noDupAIs' _ [] = []
        noDupAIs' prev (x@(AnnotationInfo _ ix):xs)
            | ix `elem` prev = noDupAIs' prev xs
            | otherwise = x:noDupAIs' (ix:prev) xs

intersection_non_empty :: AnnotationRule
intersection_non_empty im strand i@(IM.Interval sS sE)
    | sE <= sS = []
    | otherwise = let
        candidates = IM.overlapsWithKeys i  im
        strandFiltered = filter (matchStrand strand . snd) candidates
        subim = IM.fromList strandFiltered
        hits = filter (not . null) . map (flip IM.lookup subim) $ [sS..(sE-1)]
    in noDupAIs . intersection $ hits


-- This is a pretty terrible implementation, but the expectation is that its
-- arguments will be very small lists (1-5 elements)
intersection :: Eq a => [[a]] -> [a]
intersection [] = []
intersection [x] = x
intersection (x:xs) = intersection' x xs
    where
        -- early bail-out (which is why this is not strictly an instance of foldl1)
        intersection' [] _ = []
        intersection' y [] = y
        intersection' y (z:zs) = intersection' (common y z) zs
        common :: Eq a => [a] -> [a] -> [a]
        common [] _ = []
        common (y:ys) zs
            | y `elem` zs = y:common ys zs
            | otherwise = common ys zs


lookupFilePath context name args = case lookup name args of
    Nothing -> return Nothing
    Just a -> stringOrTypeError context a >>= (expandPath . T.unpack)

