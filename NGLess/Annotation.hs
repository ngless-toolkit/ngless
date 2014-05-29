{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Annotation
    ( intervals
    , getAnnotatStats
    , annotate
    , filterFeatures
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Text as T

import qualified Data.Vector.Unboxed as V

import Data.Maybe (fromMaybe)
import Data.Foldable(traverse_)

import Control.Monad.ST

import Data.GFF
import Data.Sam

import VectorOperations
import Language


intervals :: [GffLine] -> (IM.IntervalMap Int (S.ByteString, Int), Int)
intervals = foldl insertg (IM.empty, 0)
    where
        insertg (im,k) g = (IM.insert (asInterval g) (genId g, k) im, k + 1)
        asInterval g = IM.Interval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g


getAnnotatStats (NGOString gffFp) samFp feats = do
    gff <- L8.readFile (T.unpack gffFp)
    sam <- L8.readFile samFp
    let imGff = intervals . readAnnotations $ gff
        res = compStatsAnnot imGff sam
    putStrLn $ (show $ feats)
--    traverse_ (\(n,k) -> putStrLn ((show n) ++ " " ++ (show $ V.unsafeIndex res k) ++ " " ++ (show k))) (fst imGff)

getAnnotatStats g s f = error ((show g) ++ (show s))

genesAsIntervalMap :: L.ByteString -> (IM.IntervalMap Int (S.ByteString, Int), Int)
genesAsIntervalMap = intervals . filter ((==GffGene) . gffType) . readAnnotations


compStatsAnnot (annots, lim) sam = runST $ do
    idCounts <- zeroVec lim -- keep results by index
    mapM_ (update annots idCounts) sams
    V.freeze idCounts >>= return 
   where
    sams = readAlignments sam


update annots counts !samLine = do
    let res = IM.intersections (interval samLine) annots -- [(x,1) (y,3)]
    mapM_ (\(_,(_,k)) -> incVec counts k) res
  where
    interval y = IM.Interval (samPos y) (samPos y + samTLen y)


annotate :: FilePath -> Maybe NGLessObject -> Maybe NGLessObject -> IO ()
annotate samFP (Just gffFP) feats = getAnnotatStats gffFP samFP feats

annotate s g f = error((show s) ++ (show g) ++ (show f))


filterFeatures Nothing    _ = True
filterFeatures (Just (NGOList gff)) g = foldl (\a b -> a || b) False (map (filterFeatures' g) gff)

filterFeatures' g (NGOSymbol "gene") = (==GffGene) . gffType $ g
filterFeatures' g (NGOSymbol "exon") = (==GffExon) . gffType $ g
filterFeatures' g (NGOSymbol "cds" ) = (==GffCDS) . gffType  $ g
filterFeatures' g f = error ("not yet implemented feature" ++ (show g) ++ " " ++ (show f))