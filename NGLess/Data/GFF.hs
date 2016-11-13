{-# LANGUAGE OverloadedStrings #-}

module Data.GFF
    ( GffLine(..)
    , GffStrand(..)
    , readGffLine
    , _parseGffAttributes
    , _trimString
    ) where

import Data.Maybe
import Control.DeepSeq

import qualified Data.ByteString as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as S8

import NGLess.NGError

data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Read, Enum)

instance NFData GffStrand where
    rnf !_ = ()

data GffLine = GffLine
            { gffSeqId :: !B.ByteString
            , gffSource :: !B.ByteString
            , gffType :: !B.ByteString
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

_parseGffAttributes :: S.ByteString -> [(S.ByteString, S.ByteString)]
_parseGffAttributes = map (\(aid,aval) -> (aid, S8.filter (/='\"') . S.tail $ aval))
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
gffGeneId g = fromMaybe "unknown" (listToMaybe . catMaybes $ map (`lookup` attrs) ["ID", "gene_id"])
    where
        attrs = _parseGffAttributes g


readGffLine :: B.ByteString -> Either NGError GffLine
readGffLine line
    |length tokens == 9 = return $ GffLine
                tk0
                tk1
                tk2
                (read $ B8.unpack tk3)
                (read $ B8.unpack tk4)
                (score tk5)
                (parseStrand $ B8.head tk6)
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

parseStrand :: Char -> GffStrand
parseStrand '.' = GffUnStranded
parseStrand '+' = GffPosStrand
parseStrand '-' = GffNegStrand
parseStrand '?' = GffUnknownStrand
parseStrand _ = error "unhandled value for strand"
