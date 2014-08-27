{-# LANGUAGE OverloadedStrings #-}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , srLength
    , guessEncoding
    , encodingOffset
    , encodingName
    , parseReadSet
    , showRead
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import Data.Word

data ShortRead = ShortRead
        { srHeader :: !B.ByteString
        , srSequence :: !B.ByteString
        , srQualities :: !B.ByteString
        } deriving (Eq, Show, Ord)

data FastQEncoding = SangerEncoding | SolexaEncoding deriving (Eq, Bounded, Enum, Show, Ord)

srLength = B.length . srSequence

encodingOffset :: Num a => FastQEncoding -> a
encodingOffset SangerEncoding = 33
encodingOffset SolexaEncoding = 64

encodingName :: FastQEncoding -> String
encodingName SangerEncoding = "Sanger (also recent Illumina)"
encodingName SolexaEncoding = "Solexa (older Illumina)"

guessEncoding :: Word8 -> FastQEncoding
guessEncoding lowC
    | lowC < 33 = error ("No known encodings with chars < 33 (Yours was "++ (show lowC) ++ ")")
    | lowC < 64 = SangerEncoding
    | otherwise = SolexaEncoding


parseReadSet :: FastQEncoding -> BL.ByteString -> [ShortRead]
parseReadSet enc contents = parse' . map BL.toStrict . BL.lines $ contents
    where
        parse' [] = []
        parse' (lid:lseq:_:lqs:xs) = (createRead lid lseq lqs) : parse' xs
        parse' _ = error "Number of lines is not multiple of 4!"
        createRead rid rseq rqs = ShortRead rid rseq (decodeQual rqs)
        offset = encodingOffset enc
        decodeQual = B.map sub
        sub v = v - offset

showRead :: FastQEncoding -> ShortRead -> BL.ByteString
showRead enc (ShortRead a b c) = BL.fromChunks [a, "\n", b, "\n+\n", encodeQual c]
    where
        encodeQual = B.map ((+) (encodingOffset enc))


