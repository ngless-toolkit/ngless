{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Vector
    ( tgroup_Vector
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Utils.Vector

tgroup_Vector = $(testGroupGenerator)

-- Pure Validation

case_binarySearch1 = binarySearch (V.fromList [1,2,5]) ( 1 :: Int) @?= 0
case_binarySearch2 = binarySearch (V.fromList [1,2,5]) ( 2 :: Int) @?= 1
case_binarySearch3 = binarySearch (V.fromList [1,2,5]) ( 4 :: Int) @?= 2
case_binarySearch4 = binarySearch (V.fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch5 = binarySearch (V.fromList [1,2,5]) ( 5 :: Int) @?= 2
case_binarySearch6 = binarySearch (V.fromList [1,2,5]) (15 :: Int) @?= 3
case_binarySearch7 = binarySearch (V.fromList [1,2,5]) (-1 :: Int) @?= 0

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (a:b:rs) = a <= b && isSorted (b:rs)

sortList :: [Int] -> [Int]
sortList vs = unsafePerformIO $ do
        v <- V.unsafeThaw (V.fromList vs)
        sortParallel 4 v
        V.toList <$> V.unsafeFreeze v
prop_sortP vs = isSorted (sortList vs)

