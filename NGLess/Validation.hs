{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Validation
    ( validate
    ) where

import Language

import Data.Either
import qualified Data.ByteString as S

validate :: Expression -> Either S.ByteString Expression
validate = Right
