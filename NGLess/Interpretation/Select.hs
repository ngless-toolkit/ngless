{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Interpretation.Select
    ( executeSelect
    , _parseConditions
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Control.Monad
import Control.Applicative ((<$>))
import System.IO
import Utils.Utils

import Language
import FileManagement

import Data.Sam

data SelectCondition = SelectMapped | SelectUnmapped
    deriving (Eq, Show)

_parseConditions :: [(T.Text, NGLessObject)] -> ([SelectCondition], [SelectCondition])
_parseConditions args =
    let asSC (NGOSymbol "mapped") = SelectMapped
        asSC (NGOSymbol "unmapped") = SelectUnmapped
        asSC c = error ("Check failed.  Should not have seen this condition: '" ++ show c ++ "'")
        NGOList keep_if = lookupWithDefault (NGOList []) "keep_if" args
        NGOList drop_if = lookupWithDefault (NGOList []) "drop_if" args
        keep_if' = map asSC keep_if
        drop_if' = map asSC drop_if
    in (keep_if', drop_if')

_matchConditions :: ([SelectCondition], [SelectCondition]) -> SamLine -> Bool
_matchConditions ([], []) _  = True
_matchConditions ([], drop_if) samline = none (_match1 samline) drop_if
    where none f = not . any f
_matchConditions (keep_if, []) samline = all (_match1 samline) keep_if
_matchConditions _ _ = error "Either `keep_if` or `drop_if` must be empty"

_match1 samline SelectMapped = isAligned samline
_match1 samline SelectUnmapped = not $ isAligned samline

executeSelect (NGOMappedReadSet fpsam ref) args = do
    let conditions = _parseConditions args
    (oname,ohand) <- openNGLTempFile fpsam "selected_" "sam"
    samcontents <- BL.lines <$> BL.readFile fpsam
    forM_ samcontents $ \line ->
        when (BL.take 1 line == "@" ||
            _matchConditions conditions (readSamLine line))
                (BL.hPut ohand line >> BL.hPut ohand "\n")
    hClose ohand
    return (NGOMappedReadSet oname ref)
executeSelect o _ = error ("NGLESS type checking error (Select received " ++ show o ++ ")")
