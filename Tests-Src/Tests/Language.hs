{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Language
    ( tgroup_Language
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language

tgroup_Language = $(testGroupGenerator)

case_staticValue1 =
    staticValue (ListExpression [ConstStr "Hello"
                    , BinaryOp BOpPathAppend
                                (ConstStr "results-dir")
                                (ConstStr "output.txt")])
        @=? (Just $ NGOList [NGOString "Hello", NGOString "results-dir/output.txt"])

case_staticValue2 = do
    staticValue (ListExpression [ConstStr "Hello"
                    , BinaryOp BOpPathAppend
                                (ConstStr "results-dir")
                                (ConstStr "output.txt")
                    , Lookup Nothing (Variable "nope")])
        @=? Nothing
