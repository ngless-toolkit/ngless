{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Vector
    ( tgroup_Vector
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

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

newtype BigList = BigList [Int]
    deriving (Eq, Show)
instance Arbitrary BigList where
    arbitrary = BigList <$> resize (1024*16) arbitrary

prop_sortP (BigList vs) = isSorted (sortList vs)

case_pivot_extreme = assertBool "sort list pivot" (isSorted . sortList $ [10] ++ [0 | _ <- [0 :: Int ..10000]] ++ [1 :: Int])

