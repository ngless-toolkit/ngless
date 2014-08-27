{-# LANGUAGE OverloadedStrings #-}

module Data.FastQ
    ( ShortRead(..)
    , FastQEncoding(..)
    , srLength
    , guessEncoding
    , encodingOffset
    , encodingName
    , parseFastQ
    , asFastQ
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
    | lowC < 58 = SangerEncoding
    | otherwise = SolexaEncoding


parseFastQ :: FastQEncoding -> BL.ByteString -> [ShortRead]
parseFastQ enc contents = parse' . map BL.toStrict . BL.lines $ contents
    where
        parse' [] = []
        parse' (lid:lseq:_:lqs:xs) = (createRead lid lseq lqs) : parse' xs
        parse' xs = error (concat ["Number of lines is not multiple of 4! EOF:" ++ show xs])
        createRead rid rseq rqs = ShortRead rid rseq (decodeQual rqs)
        offset = encodingOffset enc
        decodeQual = B.map sub
        sub v = v - offset

asFastQ :: FastQEncoding -> [ShortRead] -> BL.ByteString
asFastQ enc rs = BL.fromChunks (asFastQ' rs)
    where
        asFastQ' [] = []
        asFastQ' ((ShortRead a b c):rss) = [a, "\n", b, "\n+\n", encodeQual c, "\n"] ++ asFastQ' rss
        encodeQual = B.map ((+) (encodingOffset enc))


