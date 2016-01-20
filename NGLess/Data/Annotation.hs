{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}
module Data.Annotation
    ( AnnotatedRead(..)
    , encodeAR
    , decodeAR
    ) where

import Control.Monad
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import Data.GFF

data AnnotatedRead = AnnotatedRead
    { readId :: !B.ByteString
    , annotValue :: !B.ByteString
    , annotType :: !GffType
    , annotStrand :: !GffStrand
    } deriving (Eq,Show)

instance NFData AnnotatedRead where
    rnf !_ = ()

encodeStrand GffPosStrand = "+"
encodeStrand GffNegStrand = "-"
encodeStrand GffUnknownStrand = "?"
encodeStrand GffUnStranded = "."

decodeStrand "+" = return GffPosStrand
decodeStrand "-" = return GffNegStrand
decodeStrand "?" = return GffUnknownStrand
decodeStrand "." = return GffUnStranded
decodeStrand  _ = fail "Ngless bug"

encodeAR :: AnnotatedRead -> B.ByteString
encodeAR (AnnotatedRead rid v t s) = B.concat
        [rid, "\t", v, "\t",  B8.pack (show t), "\t", encodeStrand s, "\n"]

decodeAR :: BL.ByteString -> Either String AnnotatedRead
decodeAR a = do
    let ilines = BL8.lines a
    when (length ilines /= 1) $
        fail "decodeAR expected a single line"
    let [line] = ilines
        tokens = BL8.split '\t' line
    when (length tokens /= 4) $
        fail "decodeAR wrong number of tokens"
    let [rid, avalue, atype, astrand] = tokens
    strand <- decodeStrand astrand
    return (AnnotatedRead (BL8.toStrict rid) (BL8.toStrict avalue) (read . BL8.unpack $ atype) strand)

