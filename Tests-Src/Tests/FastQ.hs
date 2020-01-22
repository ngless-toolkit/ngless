{- Copyright 2013-2020 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
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
import qualified Data.Vector.Storable as VS
import           Data.Int
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Interpretation.FastQ
import Interpretation.Substrim
import Tests.Utils
import Data.FastQ
import Utils.Here
import Utils.Conduit
import NGLess.NGError

tgroup_FastQ = $(testGroupGenerator)

case_parse_encode_sanger = encodeRecover SangerEncoding @?= Right reads3
case_parse_encode_solexa = encodeRecover SolexaEncoding @?= Right reads3


-- | reads a sequence of short reads.
-- returns an error if there are any problems.
--
-- Note that the result of this function is not lazy! It will consume the whole
-- input before it produces the first output (because it needs to determine
-- whether an error occurred).
fqDecode :: FastQEncoding -> BL.ByteString -> NGLess [ShortRead]
fqDecode enc s = C.runConduit $
    CL.sourceList (BL.toChunks s)
        .| linesC
        .| fqDecodeC enc
        .| CL.consume

encodeRecover enc = fqDecode enc . BL.fromChunks $ map (fqEncode enc) reads3
reads3 =
    [ShortRead "x" "acttg" (VS.fromList [35,16,34,34,24])
    ,ShortRead "y" "catgt" (VS.fromList [33,17,25,37,18])
    ,ShortRead "z" "ccggg" (VS.fromList [32,16,20,32,17])
    ]

simpleStats f = testNGLessIO $ do
    enc <- encodingFor f
    qualityPercentiles <$> statsFromFastQ f enc

case_compatibleHeader = do
    assertBool "remove /[12]" $ compatibleHeader "@SRR4052021.40730 4073/1" "@SRR4052021.40730 4073/2"
    assertBool "equal headers" $ compatibleHeader "@SRR4052021.40730 4073" "@SRR4052021.40730 4073"
    assertBool "not equal " $ not $ compatibleHeader "@SRR4052021.40730 4073" "@SRR4052021.40730 4074"
    assertBool "suffix is not /[12]" $ not $ compatibleHeader "@SRR4052021.40730 4073 xa" "@SRR4052021.40730 4073 xb"

-- negative tests quality on value 60 char ';'. Value will be 60 - 64 which is -4
case_calc_statistics_negative = do
    lowQ <- testNGLessIO $ asTempFile low_quality "fq"
    s <- simpleStats lowQ
    head s @?= (-4,-4,-4,-4)

-- low positive tests quality on 65 char 'A'. Value will be 65-64 which is 1.
case_calc_statistics_low_positive = do
    lowQ <- testNGLessIO $ asTempFile low_quality "fq"
    s <- simpleStats lowQ
    last s @?= (1,1,1,1)


low_quality = [here|
@IRIS:7:1:17:394#0/1
GTCAGGACAAT
+
<=>?@ABCAWA
@IRIS:7:1:17:800#0/1
GGAAACACTA
+
<=>?@ABCAA
|]

case_calc_statistics_normal = do
    s <- simpleStats "test_samples/data_set_repeated.fq"
    head s @?= (25,33,31,33)


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


newtype VS8 = VS8 (VS.Vector Int8)
    deriving (Eq, Show)

instance Arbitrary VS8 where
    arbitrary = VS8 . VS.fromList <$> listOf1 (oneof (return <$> [-5 .. 40]))

-- Test SUBSTRIM/ENDSTRIM
--Property 1: For every s, the size must be always smaller than the input
prop_trim_maxsize m (VS8 s) = st >= 0 && e <= VS.length s
    where (st,e) = genericTrimPos m s 20

-- Property 2: substrim should be idempotent
prop_strim_idempotent m (VS8 s) = st == 0 && n == VS.length s1
    where
        (st0, n0) = genericTrimPos m s 20
        s1 = VS.slice st0 n0 s
        (st,n) = genericTrimPos m s1 20

data SplitVU8 = SplitVU8 (VS.Vector Int8) Int Int
    deriving (Show)

instance Arbitrary SplitVU8 where
    arbitrary = do
        qs <- VS.fromList <$> listOf1 arbitrary
        st <- elements [0 .. VS.length qs - 1]
        n <- elements [0 .. VS.length qs - st]
        return $! SplitVU8 qs st n

prop_substrim_no_better (SplitVU8 qs s n) = not valid || n' >= n
    where
        thr = 20
        valid = VS.all (> thr) (VS.slice s n qs)
        (_, n') = subtrimPos qs thr

case_substrim_normal_exec =  subtrimPos (VS.fromList [10,11,12,123,122,111,10,11,0]) 20 @?= (3,3)
case_substrim_empty_quals = subtrimPos VS.empty 20 @?= (0,0)

case_endstrim_quals_allbad = snd (endstrimPos EndstrimBoth (VS.fromList [1,2,2,2,2,1,2,2]) 10) @?= 0
case_endstrim_quals_allbad_tresh = snd (endstrimPos EndstrimBoth (VS.fromList [9,9,9,9]) 10) @?= 0
case_endstrim_quals_allOK_tresh = endstrimPos EndstrimBoth (VS.fromList [10,10,10,10]) 10 @?= (0,4)
case_endstrim_quals_one_OK = endstrimPos EndstrimBoth (VS.fromList [9,9,10,9]) 10 @?= (2,1)
case_endstrim_quals_one_OK_3 = endstrimPos Endstrim3 (VS.fromList [9,9,10,9]) 10 @?= (0,3)
case_endstrim_quals_one_OK_5 = endstrimPos Endstrim5 (VS.fromList [9,9,10,9]) 10 @?= (2,2)
case_endstrim_1 = endstrimPos EndstrimBoth (VS.fromList [9,9,10,9,9,10,9]) 10 @?= (2,4)

case_smoothtrim_empty_quals = smoothtrimPos 4 VS.empty 20 @?= (0,0)
case_smoothtrim_normal_exec =  smoothtrimPos 4 (VS.fromList [10,11,12,123,122,111,10,11,0]) 20 @?= (1,6)
case_smoothtrim_middle_bad = smoothtrimPos 4 (VS.fromList [32,32,14,32,32,14,14,2,14,14,32,32]) 20 @?= (0,5)
case_smoothtrim_isolated = smoothtrimPos 4 (VS.fromList [32,32,32,1,32,32,32]) 20 @?= (0,7)
case_smoothtrim_window_4 = smoothtrimPos 4 (VS.fromList [24,2,24,24,24,24,2,24,24,24,24,2,24]) 20 @?= (3,1)
case_smoothtrim_window_10 = smoothtrimPos 10 (VS.fromList [24,2,24,24,24,24,2,24,24,24,24,2,24]) 20 @?= (0,13)
