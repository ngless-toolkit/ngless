{- Copyright 2013-2019 NGLess Authors
 - License: MIT -}
{-# LANGUAGE CPP #-}

module Data.GFF
    ( GffLine(..)
    , GffStrand(..)
    , readGffLine
#if IS_BUILDING_TEST
    , _parseGffAttributes
    , _trimString
#endif
    ) where

import Control.Monad
import Control.DeepSeq
import           Control.Arrow (second)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lex.Integral as I
import qualified Data.ByteString.Lex.Fractional as F

import NGLess.NGError

data GffStrand = GffPosStrand | GffNegStrand | GffUnknownStrand | GffUnStranded
            deriving (Eq, Show, Enum)

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
            , gffPhase :: {-# UNPACK #-} !Int -- ^phase: use -1 to denote .
            , gffAttrs :: ![(B.ByteString, B.ByteString)]
            } deriving (Eq,Show)

instance NFData GffLine where
    -- All but the score and attrs are bang annotated
    rnf GffLine{ gffScore = sc, gffAttrs = attrs } = rnf sc `seq` rnf attrs

_parseGffAttributes :: B.ByteString -> [(B.ByteString, B.ByteString)]
_parseGffAttributes = foldMap (\(a, b) -> zip (repeat a) (B8.split ',' b))
                        . map (second (B8.filter (/='\"') . B.tail))
                        . map (\x -> B8.break (== (checkAttrTag x)) x)
                        . map _trimString
                        . B8.split ';'
                        . removeLastDel
                        . _trimString

    where
        removeLastDel :: B8.ByteString -> B8.ByteString
        removeLastDel s
            | B.null s = s
            | otherwise = case B8.last s of
                ';' -> B8.init s
                _   -> s

--Check if the atribution tag is '=' or ' '
checkAttrTag :: B.ByteString -> Char
checkAttrTag s = case B8.elemIndex '=' s of
    Nothing -> ' '
    _       -> '='

-- remove ' ' from begining and end.
_trimString :: B.ByteString -> B.ByteString
_trimString = trimBeg . trimEnd
    where
        trimBeg = B8.dropWhile isSpace
        trimEnd = fst . B8.spanEnd isSpace
        isSpace = (== ' ')


readGffLine :: B.ByteString -> Either NGError GffLine
readGffLine line = case B8.split '\t' line of
        [tk0,tk1,tk2,tk3,tk4,tk5,tk6,tk7,tk8] ->
            GffLine
                tk0
                tk1
                tk2
                <$> intOrError tk3
                <*> intOrError tk4
                <*> score tk5
                <*> strandOrError tk6
                <*> phase tk7
                <*> pure (_parseGffAttributes tk8)
        _ -> throwDataError ("unexpected line in GFF: " ++ show line)
    where
        parseOrError :: (a -> Maybe b) -> a -> NGLess b
        parseOrError p s = case p s of
                    Just v -> return v
                    Nothing -> throwDataError $ "Could not parse GFF line: "++ show line
        intOrError :: B.ByteString -> NGLess Int
        intOrError = parseOrError (liftM fst . I.readDecimal)
        floatOrError = parseOrError (liftM fst . F.readDecimal)
        score :: B.ByteString -> NGLess (Maybe Float)
        score "." = return Nothing
        score v = Just <$> floatOrError v

        phase :: B.ByteString -> NGLess Int
        phase "." = return (-1)
        phase r = intOrError r
        strandOrError :: B.ByteString -> NGLess GffStrand
        strandOrError s = case B8.uncons s of
            Just (s',_) -> parseStrand s'
            _ -> throwDataError "Could not parse GFF line (empty strand field)"

parseStrand :: Char -> NGLess GffStrand
parseStrand '.' = return GffUnStranded
parseStrand '+' = return GffPosStrand
parseStrand '-' = return GffNegStrand
parseStrand '?' = return GffUnknownStrand
parseStrand u = throwDataError $ "Parsing GFF line: unhandled value for strand ("++[u]++")"
