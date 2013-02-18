{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as S8
import Data.Either

import Language
import Parse

-- The main driver is automatically generated

main = $(defaultMainGenerator)

case_parse_symbol = parsengless "test" ":symbol:" @?= Right (Sequence [ConstSymbol "symbol"])

