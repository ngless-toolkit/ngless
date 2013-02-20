{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Data.Either

import Language
import Parse

-- The main test driver is automatically generated
main = $(defaultMainGenerator)


-- Test Parsing Module
case_parse_symbol = parsengless "test" ":symbol:" @?= Right (Sequence [ConstSymbol "symbol"])
case_parse_fastq = parsengless "test" fastqcalls @?= Right fastqcall
    where
        fastqcalls = "fastq(\"input.fq\")"
        fastqcall  = Sequence [FunctionCall Ffastq [ConstStr "input.fq"] [] Nothing]

case_parse_assignment =  parsengless "test" "reads = \"something\"" @?=
        Right (Sequence [Assignment (Variable "reads") (ConstStr "something")])


case_parse_sequence = parsengless "test" seqs @?= Right seqr
    where
        seqs = "reads = 'something'\nreads = 'something'"
        seqr = Sequence [a,a]
        a    = Assignment (Variable "reads") (ConstStr "something")

