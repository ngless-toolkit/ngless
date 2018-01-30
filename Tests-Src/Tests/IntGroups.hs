{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.IntGroups
    ( tgroup_IntGroups
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
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

