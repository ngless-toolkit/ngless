{-# LANGUAGE OverloadedStrings #-}

module Annotation
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    , gffGeneId
    , intervals
    , compStatsAnnot
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IntervalMap.FingerTree as IM

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.Maybe (fromMaybe)

import Control.DeepSeq
import Control.Monad.ST

import SamBamOperations


data GffType = GffExon
                | GffGene
                | GffCDS
                | GffOther S.ByteString
            deriving (Eq, Show)

data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Enum)


data GffLine = GffLine
            { gffSeqId :: S.ByteString
            , gffSource :: S.ByteString
            , gffType :: GffType
            , gffStart :: Int
            , gffEnd :: Int
            , gffScore :: Maybe Float
            , gffString :: GffStrand
            , gffPhase :: Int -- ^phase: use -1 to denote .
            , gffAttributes :: S.ByteString
            } deriving (Eq,Show)


parseGffAttributes :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseGffAttributes = map (\(aid,aval) -> (aid,S.tail aval))
                        . map (S8.break (=='='))
                        . S8.split ';'

instance NFData GffLine where
    rnf gl = (gffSeqId gl) `seq`
            (gffSource gl) `seq`
            (gffType gl) `seq`
            (gffStart gl) `seq`
            (gffEnd gl) `seq`
            (gffScore gl) `deepseq`
            (gffString gl) `seq`
            (gffPhase gl) `seq`
            (gffAttributes gl) `seq`
            ()

gffGeneId g = lookup (S8.pack "ID") (parseGffAttributes $ gffAttributes g)

intervals :: [GffLine] -> (IM.IntervalMap Int (S.ByteString, Int), Int)
intervals = foldl insertg (IM.empty, 0)
    where
        insertg (im,k) g = (IM.insert (asInterval g) (genId g, k) im, k + 1)
        asInterval g = IM.Interval (gffStart g) (gffEnd g)
        genId g = fromMaybe (S8.pack "unknown") $ gffGeneId g


searchAnnotation :: Int -> IM.IntervalMap Int a -> [(IM.Interval Int, a)]
searchAnnotation = IM.search 


getAnnotatStats gffFp samFp = do
    gff <- L8.readFile gffFp
    sam <- L8.readFile samFp
    return $ compStatsAnnot gff sam

readAnnotations :: L.ByteString -> [GffLine]
readAnnotations = readAnnotations' . L8.lines

genesAsIntervalMap :: L.ByteString -> (IM.IntervalMap Int (S.ByteString, Int), Int)
genesAsIntervalMap = intervals . filter ((==GffGene) . gffType) . readAnnotations

readAnnotations' :: [L.ByteString] -> [GffLine]
readAnnotations' [] = []
readAnnotations' (l:ls) = case L8.head l of
                '#' -> readAnnotations' ls
                '>' -> []
                _ -> (readLine l:readAnnotations' ls)

readLine :: L.ByteString -> GffLine
readLine line = if length tokens == 9
            then GffLine
                (strict tk0)
                (strict tk1)
                (parsegffType $ strict tk2)
                (read $ L8.unpack tk3)
                (read $ L8.unpack tk4)
                (score tk5)
                (strand $ L8.head tk6)
                (phase tk7)
                (strict tk8)
            else error (concat ["unexpected line in GFF: ", show line])
    where
        tokens = L8.split '\t' line
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8] = tokens
        parsegffType "exon" = GffExon
        parsegffType "gene" = GffGene
        parsegffType "CDS" = GffCDS
        parsegffType t = GffOther t
        score "." = Nothing
        score v = Just (read $ L8.unpack v)
        strand '.' = GffUnStranded
        strand '+' = GffPosStrand
        strand '-' = GffNegStrand
        strand '?' = GffUnknownStrand
        strand _ = error "unhandled value for strand"
        phase "." = -1
        phase r = read (L8.unpack r)


strict :: L.ByteString -> S.ByteString
strict = S.concat . L.toChunks


compStatsAnnot gff sam = runST $ do
    idCounts <- zeroVec lim -- keep results by index
    mapM_ (update annots idCounts) sams
    V.freeze idCounts >>= return
   where
    (annots, lim) = intervals . readAnnotations $ gff
    sams = readAlignments sam


update annots counts samLine = do
    let res = searchAnnotation (samPos samLine) annots -- [(x,1) (y,3)]
    mapM_ (\(_,(_,k)) -> incVec counts k) res

incVec v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec 0
    return vec