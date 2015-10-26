{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.FastQ
    ( tgroup_FastQ
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Tests.Utils
import Data.FastQ

tgroup_FastQ = $(testGroupGenerator)

case_parse_encode_sanger = parse_encode SangerEncoding @?= reads3
case_parse_encode_solexa = parse_encode SolexaEncoding @?= reads3

parse_encode enc = parseFastQ enc $ asFastQ enc reads3
reads3 =
    [ShortRead "x" "acttg" "\x23\x10\x22\x22\x18"
    ,ShortRead "y" "catgt" "\x21\x11\x19\x25\x12"
    ,ShortRead "z" "ccggg" "\x20\x10\x14\x20\x11"]

case_calculateEncoding_sanger = fromRight (guessEncoding 55) @?= SangerEncoding
case_calculateEncoding_illumina_1 = fromRight (guessEncoding 65) @?= SolexaEncoding
case_calculateEncoding_illumina_1_5 = fromRight (guessEncoding 100) @?= SolexaEncoding

