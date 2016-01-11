{-# LANGUAGE OverloadedStrings #-}

module Data.GFF
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    , gffGeneId
    , readGffLine
    , parseGffAttributes
    , _trimString
    , parsegffType
    , strand
    , showStrand
    ) where

import Data.Maybe
import Control.DeepSeq

import qualified Data.ByteString as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as S8

import NGLess.NGError

data GffType = GffExon
                | GffGene
                | GffCDS
                | GffSeqName
                | GffOther !B.ByteString
            deriving (Eq, Ord)

instance Show GffType where
    show GffExon = "exon"
    show GffGene = "gene"
    show GffCDS  = "CDS"
    show GffSeqName = "seqname"
    show (GffOther b) = B8.unpack b

instance Read GffType where
    readsPrec _ = r
        where
            r "exon" = [(GffExon, "")]
            r "gene" = [(GffGene, "")]
            r "CDS" = [(GffCDS, "")]
            r "seqname" = [(GffSeqName, "")]
            r other = [(GffOther (S8.pack other), "")]

data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Read, Enum)


data GffLine = GffLine
            { gffSeqId :: !B.ByteString
            , gffSource :: !B.ByteString
            , gffType :: !GffType
            , gffStart :: !Int
            , gffEnd :: !Int
            , gffScore :: !(Maybe Float)
            , gffStrand :: !GffStrand
            , gffPhase :: !Int -- ^phase: use -1 to denote .
            , gffId :: !B.ByteString
            } deriving (Eq,Show)

instance NFData GffLine where
    -- All but the score are bang annotated
    rnf GffLine{ gffScore = sc } = rnf sc `seq` ()

instance NFData GffType where
    rnf a = a `seq` ()

parseGffAttributes :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseGffAttributes = map (\(aid,aval) -> (aid, S8.filter (/='\"') . S.tail $ aval))
                        . map (\x -> S8.break (== (checkAttrTag x)) x)
                        . map _trimString
                        . S8.split ';'
                        . removeLastDel
                        . _trimString

    where
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
        trimBeg = B8.dropWhile isSpace
        trimEnd = fst . B8.spanEnd isSpace
        isSpace = (== ' ')


gffGeneId :: S8.ByteString -> S8.ByteString
gffGeneId g = fromMaybe "unknown" (listToMaybe . catMaybes $ map (`lookup` r) ["ID", "gene_id"])
    where
        r = parseGffAttributes g


readGffLine :: B.ByteString -> Either NGError GffLine
readGffLine line
    |length tokens == 9 = return $ GffLine
                tk0
                tk1
                (parsegffType tk2)
                (read $ B8.unpack tk3)
                (read $ B8.unpack tk4)
                (score tk5)
                (strand $ B8.head tk6)
                (phase tk7)
                (S8.copy . gffGeneId $ tk8)
    |otherwise = throwDataError ("unexpected line in GFF: " ++ show line)
    where
        tokens = B8.split '\t' line
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8] = tokens
        score "." = Nothing
        score v = Just (read $ B8.unpack v)
        phase "." = -1
        phase r = read (B8.unpack r)


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
