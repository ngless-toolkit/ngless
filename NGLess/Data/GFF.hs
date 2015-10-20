{-# LANGUAGE OverloadedStrings #-}

module Data.GFF
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    , gffGeneId
    , readAnnotations
    , parseGffAttributes
    , checkAttrTag
    , _trimString
    , parsegffType
    , readLine
    , strand
    , showStrand
    ) where

import Data.Maybe
import Control.DeepSeq

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

data GffType = GffExon
                | GffGene
                | GffCDS
                | GffOther !B.ByteString
            deriving (Eq, Ord)

instance Show GffType where
    show GffExon = "exon"
    show GffGene = "gene"
    show GffCDS  = "CDS"
    show (GffOther b) = B.unpack b

instance Read GffType where
    readsPrec _ = r
        where
            r "exon" = [(GffExon, "")]
            r "gene" = [(GffGene, "")]
            r "CDS" = [(GffCDS, "")]
            r other = [(GffOther (S8.pack other), "")]

data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Read, Enum)


data GffLine = GffLine
            { gffSeqId :: S.ByteString
            , gffSource :: S.ByteString
            , gffType :: GffType
            , gffStart :: !Int
            , gffEnd :: !Int
            , gffScore :: Maybe Float
            , gffStrand :: GffStrand
            , gffPhase :: !Int -- ^phase: use -1 to denote .
            , gffId :: S.ByteString
            } deriving (Eq,Show)

instance NFData GffLine where
    rnf gl = (gffSeqId gl) `seq`
            (gffSource gl) `seq`
            (gffType gl) `seq`
            (gffStart gl) `seq`
            (gffEnd gl) `seq`
            (gffScore gl) `deepseq`
            (gffStrand gl) `seq`
            (gffPhase gl) `seq`
            (gffId gl) `deepseq`
            ()

instance NFData GffType where
    rnf a = a `seq` ()


parseGffAttributes :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseGffAttributes = map (\(aid,aval) -> (aid, S8.filter (/='\"') . S.tail $ aval))
                        . map (\x -> S8.break (== (checkAttrTag x)) x)
                        . map _trimString
                        . S8.split ';'
                        . removeLastDel
                        . _trimString


removeLastDel :: S8.ByteString -> S8.ByteString
removeLastDel s = case S8.last s of
    ';' -> S8.init s
    _   -> s

--Check if the atribution tag is '=' or ' '
checkAttrTag :: S.ByteString -> Char
checkAttrTag s = case S8.elemIndex '=' s of
    Nothing -> ' '
    _       -> '='

-- remove ' ' from begining and end.
_trimString :: S.ByteString -> S.ByteString
_trimString = trimBeg . trimEnd
    where
        trimBeg = B.dropWhile isSpace
        trimEnd = fst . B.spanEnd isSpace
        isSpace = (== ' ')


gffGeneId :: S8.ByteString -> S8.ByteString
gffGeneId g = fromMaybe "unknown" (listToMaybe . catMaybes $ map (`lookup` r) ["ID", "gene_id"])
    where
        r = parseGffAttributes g


readAnnotations :: L.ByteString -> [GffLine]
readAnnotations = readAnnotations' . L8.lines

readAnnotations' :: [L.ByteString] -> [GffLine]
readAnnotations' [] = []
readAnnotations' (l:ls) = case L8.head l of
                '#' -> readAnnotations' ls
                '>' -> []
                _ -> (readLine l:readAnnotations' ls)

readLine :: L.ByteString -> GffLine
readLine line = if length tokens == 9
            then GffLine
                (BL.toStrict tk0)
                (BL.toStrict tk1)
                (parsegffType $ BL.toStrict tk2)
                (read $ L8.unpack tk3)
                (read $ L8.unpack tk4)
                (score tk5)
                (strand $ L8.head tk6)
                (phase tk7)
                (S8.copy . gffGeneId $ BL.toStrict tk8)
            else error (concat ["unexpected line in GFF: ", show line])
    where
        tokens = L8.split '\t' line
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8] = tokens
        score "." = Nothing
        score v = Just (read $ L8.unpack v)
        phase "." = -1
        phase r = read (L8.unpack r)


parsegffType :: S.ByteString -> GffType
parsegffType "exon" = GffExon
parsegffType "gene" = GffGene
parsegffType "cds" = GffCDS
parsegffType "CDS" = GffCDS
parsegffType t = GffOther t

strand :: Char -> GffStrand
strand '.' = GffUnStranded
strand '+' = GffPosStrand
strand '-' = GffNegStrand
strand '?' = GffUnknownStrand
strand _ = error "unhandled value for strand"

showStrand :: GffStrand -> S.ByteString
showStrand GffUnStranded    = "."
showStrand GffPosStrand     = "+"
showStrand GffNegStrand     = "-"
showStrand GffUnknownStrand = "?"
