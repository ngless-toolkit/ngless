{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.IntGroups
    ( tgroup_IntGroups
    ) where

import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Vector.Unboxed as VU

import qualified Utils.IntGroups as IG

tgroup_IntGroups = $(testGroupGenerator)

case_basic = (IG.toList . IG.fromList $ vs) @?= (map VU.fromList vs)
    where
        vs = [[1],[2],[2,3,4]]

case_length = (IG.length . IG.fromList $ vs) @?= length vs
    where
        vs = [[1],[2],[2,3,4]]

prop_idem vs = (IG.toList . IG.fromList $ vs) == (map VU.fromList vs)

