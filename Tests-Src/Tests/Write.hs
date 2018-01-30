{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Write
    ( tgroup_Write
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Interpretation.Write


tgroup_Write = $(testGroupGenerator)

case_format_index = _formatFQOname "test.{index}.fq.gz" "pair.1" @?= Right "test.pair.1.fq.gz"
case_format_index_first = _formatFQOname "{index}.fq.gz" "pair.2" @?= Right "pair.2.fq.gz"
case_format_no_index = _formatFQOname "filename.fq.gz" "singles" @?= Right "filename.singles.fq.gz"
case_format_no_index_dots = _formatFQOname "filename.with.dots.fq.gz" "singles" @?= Right "filename.with.dots.singles.fq.gz"
case_format_no_index_dots_rep = _formatFQOname "filename.with.dots.fq" "pair.1" @?= Right "filename.with.dots.pair.1.fq"

