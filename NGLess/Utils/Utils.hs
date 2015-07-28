{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utils.Utils
    ( lookupWithDefault
    , readPossiblyCompressedFile
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Compression.GZip as GZip

import Control.Applicative ((<$>))

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)

lookupWithDefault :: Eq b => a -> b -> [(b,a)] -> a
lookupWithDefault def key values = fromMaybe def $ lookup key values

readPossiblyCompressedFile ::  FilePath -> IO BL.ByteString
readPossiblyCompressedFile fname
    | ".gz" `isSuffixOf` fname = GZip.decompress <$> BL.readFile fname
    | otherwise = BL.readFile fname

