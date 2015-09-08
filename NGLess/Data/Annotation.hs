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

encodeAR :: AnnotatedRead -> BL.ByteString
encodeAR (AnnotatedRead rid v t s) = BL.fromChunks
        [rid, "\t", v, "\t",  B8.pack (show t), "\t", B8.pack (show s), "\n"]

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
    return (AnnotatedRead (BL8.toStrict rid) (BL8.toStrict avalue) (read . BL8.unpack $ atype) (read . BL8.unpack $ astrand))

