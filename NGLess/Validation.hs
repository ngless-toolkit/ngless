{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Validation
    ( validate
    ) where

import Language

import Data.Maybe
import Data.Either
import qualified Data.ByteString as S

validate :: Expression -> Either S.ByteString Expression
validate expr = case errors of
        [] -> Right expr
        _ -> Left (S.concat errors)
    where
        errors = catMaybes (map ($expr) checks)
        checks = [validate_types]

{- Each checking function has the type
 -
 - Expression -> Maybe S.ByteString
 -
 - If it finds an error, it returns a Just error; otherwise, Nothing.
 -
 - The validate function just runs all checks and either concatenates all the
 - error messages or passes the expression unharmed on the Right side.
 -}
validate_types :: Expression -> Maybe S.ByteString
validate_types = const Nothing

