{-# LANGUAGE OverloadedStrings #-}

module Annotation
    ( intervals
    , getAnnotatStats
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IntervalMap.FingerTree as IM

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.Maybe (fromMaybe)
import Data.Foldable(traverse_)

import Control.DeepSeq
import Control.Monad.ST

import SamBamOperations
import Data.GFF


intervals :: [GffLine] -> (IM.IntervalMap Int (S.ByteString, Int), Int)
intervals = foldl insertg (IM.empty, 0)
    where
        insertg (im,k) g = (IM.insert (asInterval g) (genId g, k) im, k + 1)
        asInterval g = IM.Interval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g


getAnnotatStats gffFp samFp = do
    gff <- L8.readFile gffFp
    sam <- L8.readFile samFp
    let imGff = intervals . readAnnotations $ gff
        res = compStatsAnnot (imGff) sam

    traverse_ (\(n,k) -> putStrLn ((show n) ++ " " ++ (show $ V.unsafeIndex res k))) (fst imGff)


genesAsIntervalMap :: L.ByteString -> (IM.IntervalMap Int (S.ByteString, Int), Int)
genesAsIntervalMap = intervals . filter ((==GffGene) . gffType) . readAnnotations


compStatsAnnot (annots, lim) sam = runST $ do
    idCounts <- zeroVec lim -- keep results by index
    mapM_ (update annots idCounts) sams
    V.freeze idCounts >>= return 
   where
    sams = readAlignments sam


update annots counts samLine = do
    let res = IM.intersections (interval samLine) annots -- [(x,1) (y,3)]
    mapM_ (\(_,(_,k)) -> incVec counts k) res
  where
    interval y = IM.Interval (samPos y) (samPos y + samTLen y)

incVec v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec