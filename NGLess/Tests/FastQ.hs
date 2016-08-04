{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.FastQ
    ( tgroup_FastQ
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import Tests.Utils
import Data.FastQ

tgroup_FastQ = $(testGroupGenerator)

case_parse_encode_sanger = encodeRecover SangerEncoding @?= Right reads3
case_parse_encode_solexa = encodeRecover SolexaEncoding @?= Right reads3

encodeRecover enc = fqDecode enc . BL.fromChunks $ map (fqEncode enc) reads3
reads3 =
    [ShortRead "x" "acttg" (VU.fromList [35,16,34,34,24])
    ,ShortRead "y" "catgt" (VU.fromList [33,17,25,37,18])
    ,ShortRead "z" "ccggg" (VU.fromList [32,16,20,32,17])
    ]

case_calculateEncoding_sanger = fromRight (guessEncoding 55) @?= SangerEncoding
case_calculateEncoding_illumina_1 = fromRight (guessEncoding 65) @?= SolexaEncoding
case_calculateEncoding_illumina_1_5 = fromRight (guessEncoding 100) @?= SolexaEncoding

