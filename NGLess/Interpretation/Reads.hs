{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections #-}

module Interpretation.Reads
    ( executeReads
    ) where

import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<$>))
import System.IO

import Language
import FileManagement

import Data.Sam
import Data.FastQ

executeReads (NGOMappedReadSet fpsam _) _ = do
    fp <- samToFastQ fpsam
    return (NGOReadSet1 SangerEncoding fp)
executeReads _ _ = error "NGLESS type checking error"

samToFastQ :: FilePath -> IO FilePath
samToFastQ fpsam = do
    (oname,ohand) <- openNGLTempFile fpsam "reads_" "fq"
    fq <-  (map asFQ . readAlignments) <$> BL.readFile fpsam
    BL.hPut ohand (BL.concat fq)
    hClose ohand
    return oname
    
asFQ :: SamLine -> BL.ByteString
asFQ SamLine{samQName=qname, samSeq=short, samQual=qs} = BL.fromChunks ["@", qname, "\n", short, "\n+\n", qs, "\n"]
