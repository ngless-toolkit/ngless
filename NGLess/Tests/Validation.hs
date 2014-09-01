{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Validation
    ( tgroup_Validation
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit

import Tests.Utils
import Validation

tgroup_Validation = $(testGroupGenerator)

case_mismatched_argument = isError $ (validate =<< parsetest "ngless '0.0'\n\
            \input = fastq('input.fq')\n\
            \mapped = map(input, reference='hg19')\n\
            \ann = annotate(mapped, mode={deny})")

