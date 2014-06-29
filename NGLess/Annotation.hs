{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Annotation
    ( intervals
    , annotate
    , getIntervalQuery
    , isInsideInterval
    , filterByInterval
    , union
    , intersection_strict
    , intersection_non_empty
    , sizeNoDup
    ) where


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

import qualified Data.IntervalMap.Strict as IM

import qualified Data.Set as Set
import qualified Data.Map as M

import Data.Maybe (fromMaybe, fromJust)
import Data.List (foldl')

import Language
import FileManagement(printNglessLn, unCompress)
import UnpackIlluminaGenomes

import Data.GFF
import Data.Sam
import Data.AnnotRes



annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> Maybe T.Text -> Maybe NGLessObject -> Maybe NGLessObject -> IO T.Text
annotate samFP (Just g) feats _ m a = 
    printNglessLn ("annotate with GFF: " ++ (eval g) ++ (show m)) >> annotate' samFP (eval g) feats a (getIntervalQuery m)  -- ignore default GFF
    where eval (NGOString n) = T.unpack n
          eval _ = error ("Provided type for gff must be a NGOString.")

annotate samFP Nothing  feats g m  a= 
    printNglessLn ("annotate with default GFF: " ++ (show . fromJust $ g) ++ (show m)) >> 
        case g of
            Just v  -> annotate' samFP (getGff v) feats a (getIntervalQuery m)     -- used default GFF
            Nothing -> error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: Maybe NGLessObject -> ([IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount)
getIntervalQuery m = fromMaybe (union) (getMode m)

getMode m = case m of
            Nothing -> Nothing
            Just v  -> case v of
                    (NGOString "union")                  -> Just union
                    (NGOString "intersection_strict")    -> Just intersection_strict
                    (NGOString "intersection_non_empty") -> Just intersection_non_empty
                    _ -> error ("Provided type for 'mode' has to be either 'intersecting' or 'withing'")


annotate' :: FilePath -> FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> ([IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount) -> IO T.Text
annotate' samFp gffFp feats f a = do
    gff <- unCompress gffFp
    sam <- unCompress samFp
    let imGff = intervals . filter (filterFeatures feats) . readAnnotations $ gff
        counts = compStatsAnnot imGff sam f a -- Map 'feats' (Map chr (Imap key val))
    writeAnnotCount samFp $ map snd . concat . map IM.toList . concat . map M.elems . M.elems $ counts 
                                                -- [Map k v, Map k v, Map k v, Map k v] -> [[v], [v], [v], [v]] -> [v]
                                                -- v = Imap key Gffcounts


compStatsAnnot ::  M.Map S8.ByteString (M.Map S8.ByteString (IM.IntervalMap Int GffCount)) -> L8.ByteString -> Maybe NGLessObject -> ([IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount) -> M.Map S8.ByteString (M.Map S8.ByteString (IM.IntervalMap Int GffCount)) 
compStatsAnnot imGff sam a f = foldl (iterSam f a) imGff sams
   where 
    sams = filter isAligned . readAlignments $ sam


iterSam :: ([IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount) -> Maybe NGLessObject -> M.Map S8.ByteString (M.Map S8.ByteString (IM.IntervalMap Int GffCount)) -> SamLine -> M.Map S8.ByteString (M.Map S8.ByteString (IM.IntervalMap Int GffCount))
iterSam f a im y = M.map (\v -> M.alter (\x -> alterCounts x) k v) im 
    where
        alterCounts x = case x of
            Nothing -> Nothing
            Just v  -> Just $ modeAnnotation f a v y
        k = samRName y


modeAnnotation :: ([IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount) -> Maybe NGLessObject -> IM.IntervalMap Int GffCount -> SamLine -> IM.IntervalMap Int GffCount
modeAnnotation f a im y = do
    let res = f [fStart, fEnd] -- with pair ended need to add the other 2 intervals of the pair 
    countsAmbiguity a res im
  where 
    sStart = samPos y
    sEnd   = sStart + (cigarTLen $ samCigar y)
    im'    = IM.fromList $ IM.intersecting im (IM.ClosedInterval sStart sEnd) -- this allows to filter im to only the ones needed.
    fStart = filterByInterval sStart im'
    fEnd   = filterByInterval sEnd im'

countsAmbiguity :: Maybe NGLessObject -> IM.IntervalMap Int GffCount -> IM.IntervalMap Int GffCount -> IM.IntervalMap Int GffCount
countsAmbiguity (Just a) toU imR =
    case a of
        NGOSymbol "allow" -> updateWithAmb    toU imR
        NGOSymbol "deny"  -> updateWithoutAmb toU imR
        err -> error ("NGOSymbol must be allow or deny, but was passed: " ++ (show err))
countsAmbiguity Nothing toU imR = updateWithAmb toU imR

updateWithAmb toU imR = uCounts toU imR
updateWithoutAmb toU imR = 
    case IM.size imR of  
        0 -> imR --"no_feature"
        1 -> uCounts toU imR --"feature not ambiguous"
        2 -> do
            case sizeNoDup imR of --garantees that same feature separated do not count as 2
                1 -> uCounts toU imR -- same feature, increase both
                _ -> imR -- "ambiguous"
        _ -> imR --"ambiguous"


uCounts :: IM.IntervalMap Int GffCount -> IM.IntervalMap Int GffCount -> IM.IntervalMap Int GffCount 
uCounts keys im = IM.foldlWithKey (\res k _ -> IM.adjust (incCount) k res) im keys 
    where 
        incCount (GffCount gId gT gC) = (GffCount gId gT (gC + 1))


isInsideInterval :: Int -> IM.Interval Int -> Bool
isInsideInterval k (IM.ClosedInterval l u) = k >= l && k <= u  
isInsideInterval _ err = error("Expecting ClosedInterval but got: " ++ (show err))

filterByInterval :: Int -> IM.IntervalMap Int GffCount -> IM.IntervalMap Int GffCount 
filterByInterval v = IM.filterWithKey (\k _ -> isInsideInterval v k) -- O(n)


--- Diferent modes

union :: [IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount  
union = IM.unions

intersection_strict :: [IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount  
intersection_strict im = case null im of
            True  -> IM.empty
            False -> foldl (IM.intersection) (head im) im

intersection_non_empty :: [IM.IntervalMap Int GffCount] -> IM.IntervalMap Int GffCount  
intersection_non_empty im = intersection_strict . filter (not . IM.null) $ im

--------------------

sizeNoDup :: IM.IntervalMap Int GffCount -> Int
sizeNoDup im = Set.size $ IM.foldl (\m v -> Set.insert (annotSeqId v) m) Set.empty im -- 1 (mesmo feature) ou 2 features diferentes.

--------------------

intervals :: [GffLine] -> M.Map S8.ByteString (M.Map S8.ByteString (IM.IntervalMap Int GffCount))
intervals = foldl' (insertg) (M.empty)
    where
        insertg im g = M.alter (\mF -> updateF g mF) (showType . gffType $ g) im
        updateF g mF = case mF of
            Nothing  -> Just $ updateF' g M.empty  
            Just mF' -> Just $ updateF' g mF'

        updateF' g mF = M.alter (\v -> updateChrMap g v) (gffSeqId g) mF
        updateChrMap g v  = case v of
            Nothing -> Just $ IM.insert (asInterval g) (GffCount (genId g) (gffType g) 0) IM.empty
            Just a  -> Just $ IM.insert (asInterval g) (GffCount (genId g) (gffType g) 0) a

        asInterval g = IM.ClosedInterval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g

