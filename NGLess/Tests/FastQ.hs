{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.FastQ
    ( tgroup_FastQ
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import           Data.Int

import Interpretation.Substrim
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

data Method = MSubstrim | MEndstrim EndstrimEnds
    deriving (Show)

instance Arbitrary Method where
    arbitrary = oneof
                    [return MSubstrim
                    , return $ MEndstrim EndstrimBoth
                    , return $ MEndstrim Endstrim3
                    , return $ MEndstrim Endstrim5
                    ]

genericTrimPos MSubstrim = subtrimPos
genericTrimPos (MEndstrim ends) = endstrimPos ends


newtype VU8 = VU8 (VU.Vector Int8)
    deriving (Eq, Show)

instance Arbitrary VU8 where
    arbitrary = VU8 . VU.fromList <$> listOf1 (oneof (return <$> [-5 .. 40]))

-- Test SUBSTRIM/ENDSTRIM
--Property 1: For every s, the size must be always smaller than the input
prop_trim_maxsize m (VU8 s) = st >= 0 && e <= VU.length s
    where (st,e) = genericTrimPos m s 20

-- Property 2: substrim should be idempotent
prop_strim_idempotent m (VU8 s) = st == 0 && n == VU.length s1
    where
        (st0, n0) = genericTrimPos m s 20
        s1 = VU.slice st0 n0 s
        (st,n) = genericTrimPos m s1 20

data SplitVU8 = SplitVU8 (VU.Vector Int8) Int Int
    deriving (Show)

instance Arbitrary SplitVU8 where
    arbitrary = do
        qs <- VU.fromList <$> listOf1 arbitrary
        st <- elements [0 .. VU.length qs - 1]
        n <- elements [0 .. VU.length qs - st]
        return $! SplitVU8 qs st n

prop_substrim_no_better (SplitVU8 qs s n) = not valid || n' >= n
    where
        thr = 20
        valid = VU.all (> thr) (VU.slice s n qs)
        (_, n') = subtrimPos qs thr

case_substrim_normal_exec =  subtrimPos (VU.fromList [10,11,12,123,122,111,10,11,0]) 20 @?= (3,3)
case_substrim_empty_quals = subtrimPos VU.empty 20 @?= (0,0)

case_endstrim_quals_allbad = snd (endstrimPos EndstrimBoth (VU.fromList [1,2,2,2,2,1,2,2]) 10) @?= 0
case_endstrim_quals_allbad_tresh = snd (endstrimPos EndstrimBoth (VU.fromList [9,9,9,9]) 10) @?= 0
case_endstrim_quals_allOK_tresh = endstrimPos EndstrimBoth (VU.fromList [10,10,10,10]) 10 @?= (0,4)
case_endstrim_quals_one_OK = endstrimPos EndstrimBoth (VU.fromList [9,9,10,9]) 10 @?= (2,1)
case_endstrim_quals_one_OK_3 = endstrimPos Endstrim3 (VU.fromList [9,9,10,9]) 10 @?= (0,3)
case_endstrim_quals_one_OK_5 = endstrimPos Endstrim5 (VU.fromList [9,9,10,9]) 10 @?= (2,2)
case_endstrim_1 = endstrimPos EndstrimBoth (VU.fromList [9,9,10,9,9,10,9]) 10 @?= (2,4)

