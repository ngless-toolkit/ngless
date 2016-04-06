{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.IntGroups
    ( tgroup_IntGroups
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit


import Tests.Utils
import qualified Utils.IntGroups as IG

tgroup_IntGroups = $(testGroupGenerator)

case_basic = (IG.toList . IG.fromList $ vs) @?= vs
    where
        vs = [[1],[2],[2,3,4]]

case_length = (IG.length . IG.fromList $ vs) @?= length vs
    where
        vs = [[1],[2],[2,3,4]]
