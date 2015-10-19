{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.NGLessAPI
    ( tgroup_NGLessAPI
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit

import Language
import NGLess
import Tests.Utils


tgroup_NGLessAPI = $(testGroupGenerator)

case_lookupStringError = isError (lookupStringOrScriptError "test" "no" [("yes", NGOString "test")])
case_lookupStringOk = isOk "lookup failed" (lookupStringOrScriptError "test" "yes" [("yes", NGOString "test")])
case_lookupStringDefOk = isOk "default failed" (lookupStringOrScriptErrorDef (return "ok") "test" "not" [("yes", NGOString "test")])
