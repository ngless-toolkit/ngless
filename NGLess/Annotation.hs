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
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe, fromJust)
import Data.List (foldl')

import Language
import FileManagement(printNglessLn, unCompress)
import ReferenceDatabases

import Data.GFF
import Data.Sam
import Data.AnnotRes



annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> Maybe T.Text -> Maybe NGLessObject -> Maybe NGLessObject -> Maybe NGLessObject -> IO T.Text
annotate samFP (Just g) feats _ m a s = 
    printNglessLn (concat ["annotate with GFF: ", eval g])
            >> annotate' samFP (eval g) feats a (getIntervalQuery m) s  -- ignore default GFF
    where eval (NGOString n) = T.unpack n
          eval _ = error ("Provided type for gff must be a NGOString.")

annotate samFP Nothing feats dDs m a s = 
    printNglessLn (concat ["annotate with default GFF: ", show . fromJust $ dDs]) >> 
        case dDs of
            Just v  -> annotate' samFP (getGff v) feats a (getIntervalQuery m) s   -- used default GFF
            Nothing -> error("A gff must be provided by using the argument 'gff'") -- not default ds and no gff passed as arg

getIntervalQuery :: Maybe NGLessObject -> ([IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount])
getIntervalQuery m = fromMaybe (union) (getMode m)

getMode m = case m of
            Nothing -> Nothing
            Just v  -> case v of
                    (NGOSymbol "union")                  -> Just union
                    (NGOSymbol "intersection_strict")    -> Just intersection_strict
                    (NGOSymbol "intersection_non_empty") -> Just intersection_non_empty
                    _ -> error ("Provided symbol for variable 'mode' can only be 'union' or 'insersection_strict' and 'intersection_non_empty")


annotate' :: FilePath -> FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> ([IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]) -> Maybe NGLessObject -> IO T.Text
annotate' samFp gffFp feats f a s = do
    gff <- unCompress gffFp
    sam <- unCompress samFp
    let imGff = intervals . filter (filterFeatures feats) . readAnnotations $ gff
        counts = compStatsAnnot imGff sam f a s -- Map 'feats' (Map chr (Imap key val))
    writeAnnotCount samFp (toGffM . concat . map (M.elems) . M.elems $ counts)
    --writeAnnotCount samFp . IM.toList . unlines . map M.elems . M.elems $ counts 
                                                -- [Map k v, Map k v, Map k v, Map k v] -> [[v], [v], [v], [v]] -> [v]
                                                -- v = Imap key Gffcounts

toGffM :: [IM.IntervalMap Int [GffCount]] -> [GffCount]
toGffM = concat . foldl (\a b -> (++) (toGff b) a) [] 

toGff :: IM.IntervalMap Int [GffCount] -> [[GffCount]]
toGff = IM.elems

compStatsAnnot ::  M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount])) -> L8.ByteString -> Maybe NGLessObject -> ([IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]) -> Maybe NGLessObject -> M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount])) 
compStatsAnnot imGff sam a f s = foldl (iterSam f a s) imGff sams
   where 
    sams = filter isAligned . readAlignments $ sam


iterSam :: ([IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]) -> Maybe NGLessObject -> Maybe NGLessObject -> M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount])) -> SamLine -> M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount]))
iterSam f a s im y = M.map (\v -> M.alter (\x -> alterCounts x) k v) im 
    where
        alterCounts x = case x of
            Nothing -> Nothing
            Just v  -> Just $ modeAnnotation f a v y s
        k = samRName y


modeAnnotation :: ([IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]) -> Maybe NGLessObject -> IM.IntervalMap Int [GffCount] -> SamLine -> Maybe NGLessObject -> IM.IntervalMap Int [GffCount]
modeAnnotation f a im y s = countsAmbiguity a ((filterStrand s asStrand) . f $ posR) im
  where 
    sStart = samPos y
    sEnd   = sStart + (cigarTLen $ samCigar y) - 1
    posR   = map (\k -> IM.fromList $ IM.containing im k) [sStart..sEnd]
    asStrand = if isPositive y then GffPosStrand else GffNegStrand

filterStrand :: Maybe NGLessObject -> GffStrand -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
filterStrand modeS s m = maybe m (filterStrand') modeS
    where 
        filterStrand' modeS' = case modeS' of
            NGOSymbol "yes" -> IM.filter (not . null) . IM.map (filterByStrand s) $ m 
            NGOSymbol "no"  -> m -- by default no
            err              -> error ("Type must be a NGOSymbol with value 'yes' or 'no', but was passed: " ++ (show err))

countsAmbiguity :: Maybe NGLessObject -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
countsAmbiguity (Just a) toU imR =
    case a of
        NGOSymbol "allow" -> updateWithAmb    toU imR
        NGOSymbol "deny"  -> updateWithoutAmb toU imR
        err -> error ("NGOSymbol must be allow or deny, but was passed: " ++ (show err))
countsAmbiguity Nothing toU imR = updateWithAmb toU imR

updateWithAmb toU imR = uCounts toU imR
updateWithoutAmb toU imR = 
    case IM.size toU of  
        0 -> imR --"no_feature"
        _ -> case sizeNoDup toU of
            1 -> uCounts (IM.fromList . remAllButOneCount . IM.toList $ toU) imR -- same feature multiple times. increase that feature ONCE.
            _ -> imR --'ambiguous' 
    where
        remAllButOneCount = take 1 -- [(k1,[v1,v2,v3]), (k2,[v1,v2,v3])] -> [(K, _)] -- only for the case where all ids are equal.


uCounts :: IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] 
uCounts keys im = IM.foldlWithKey (\res k _ -> IM.adjust (incCount) k res) im keys 
    where 
        incCount []     = []
        incCount (x:rs) = incCount' x : rs 
        incCount' (GffCount gId gT gC gS) = (GffCount gId gT (gC + 1) gS)


isInsideInterval :: Int -> IM.Interval Int -> Bool
isInsideInterval k (IM.ClosedInterval l u) = k >= l && k <= u  
isInsideInterval _ err = error("Expecting ClosedInterval but got: " ++ (show err))

filterByInterval :: Int -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount] 
filterByInterval v = IM.filterWithKey (\k _ -> isInsideInterval v k) -- O(n)


--- Diferent modes

union :: [IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]
union = IM.unions

intersection_strict :: [IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]
intersection_strict im = case null im of
            True  -> IM.empty
            False -> foldl (IM.intersection) (head im) im

intersection_non_empty :: [IM.IntervalMap Int [GffCount]] -> IM.IntervalMap Int [GffCount]
intersection_non_empty im = intersection_strict . filter (not . IM.null) $ im

--------------------

sizeNoDup :: IM.IntervalMap Int [GffCount] -> Int
sizeNoDup im = Set.size $ IM.foldl (\m v -> Set.union m (gffIds v)) Set.empty im -- 1 (same feature) or n dif features.
    where 
        gffIds :: [GffCount] -> Set.Set S8.ByteString
        gffIds = Set.fromList . map annotSeqId
--------------------

intervals :: [GffLine] -> M.Map GffType (M.Map S8.ByteString (IM.IntervalMap Int [GffCount]))
intervals = foldl' (insertg) M.empty
    where
        insertg im g = M.alter (\mF -> updateF g mF) (gffType g) im
        updateF g mF = case mF of
            Nothing  -> Just $ updateF' g M.empty  
            Just mF' -> Just $ updateF' g mF'

        updateF' g mF = M.alter (\v -> updateChrMap g v) (gffSeqId g) mF
        updateChrMap g v  = case v of
            Nothing -> Just $ insertCount g IM.empty
            Just a  -> Just $ insertCount g a

        
insertCount :: GffLine -> IM.IntervalMap Int [GffCount] -> IM.IntervalMap Int [GffCount]
insertCount g im = IM.insertWith ((++)) (asInterval g) [GffCount (genId g) (gffType g) 0 (gffStrand g)] im


asInterval :: GffLine -> IM.Interval Int
asInterval g = IM.ClosedInterval (gffStart g) (gffEnd g)

genId :: GffLine -> S8.ByteString 
genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g

