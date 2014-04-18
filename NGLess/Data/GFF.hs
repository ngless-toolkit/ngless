{-# LANGUAGE OverloadedStrings #-}

module Data.GFF
    ( GffLine(..)
    , GffType(..)
    , GffStrand(..)
    , gffGeneId
    , readAnnotations
    ) where

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
