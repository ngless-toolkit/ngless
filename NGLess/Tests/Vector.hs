{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Vector
    ( tgroup_Vector
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Vector as V

import Tests.Utils
import Validation
import ValidationIO
import NGLess
import Control.Monad
import Utils.Vector

tgroup_Vector = $(testGroupGenerator)

-- Pure Validation

case_zero_vec = do
  v <- zeroVec 4 >>= V.freeze
  v @?= V.fromList [0,0,0,0]


case_binarySearch1 = binarySearch (fromList [1,2,5]) ( 1 :: Int) @?= 0
case_binarySearch2 = binarySearch (fromList [1,2,5]) ( 2 :: Int) @?= 1
case_binarySearch3 = binarySearch (fromList [1,2,5]) ( 4 :: Int) @?= 2
case_binarySearch4 = binarySearch (fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch5 = binarySearch (fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch6 = binarySearch (fromList [1,2,5]) (15 :: Int) @?= 3
case_binarySearch6 = binarySearch (fromList [1,2,5]) (-1 :: Int) @?= 0

