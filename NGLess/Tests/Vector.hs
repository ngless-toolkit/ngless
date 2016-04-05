{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Vector
    ( tgroup_Vector
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VGM


import Tests.Utils
import Control.Monad
import Utils.Vector

tgroup_Vector = $(testGroupGenerator)

-- Pure Validation

case_zero_vec = do
  v <- zeroVec 4 >>= VU.freeze
  v @?= VU.fromList [0,0,0,0]


case_binarySearch1 = binarySearch (V.fromList [1,2,5]) ( 1 :: Int) @?= 0
case_binarySearch2 = binarySearch (V.fromList [1,2,5]) ( 2 :: Int) @?= 1
case_binarySearch3 = binarySearch (V.fromList [1,2,5]) ( 4 :: Int) @?= 2
case_binarySearch4 = binarySearch (V.fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch5 = binarySearch (V.fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch6 = binarySearch (V.fromList [1,2,5]) (15 :: Int) @?= 3
case_binarySearch7 = binarySearch (V.fromList [1,2,5]) (-1 :: Int) @?= 0

isSorted [] = True
isSorted [_] = True
isSorted (a:b:rs) = a <= b && isSorted (b:rs)

case_sortParallel = do
    v <- V.unsafeThaw (V.fromList [0..20000 :: Int])
    sortParallel 4 v
    vs <- V.toList <$> V.unsafeFreeze v
    assertBool "sort parallel results should be sorted" (isSorted vs)

