{- Copyright 2013-2016 NGLess Authors
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

import Data.GFF

data AnnotatedRead = AnnotatedRead
    { readId :: !B.ByteString
    , annotValue :: !B.ByteString
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
encodeAR (AnnotatedRead rid v s) = B.concat
        [rid, "\t", v, "\t", encodeStrand s, "\n"]

decodeAR :: B.ByteString -> Either String AnnotatedRead
decodeAR a = do
    let ilines = B8.lines a
    when (length ilines /= 1) $
        fail "decodeAR expected a single line"
    let [line] = ilines
        tokens = B8.split '\t' line
    when (length tokens /= 4) $
        fail "decodeAR wrong number of tokens"
    let [rid, avalue, atype, astrand] = tokens
    strand <- decodeStrand astrand
    return (AnnotatedRead rid avalue strand)

