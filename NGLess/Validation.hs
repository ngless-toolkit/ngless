{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Validation
    ( validate
    ) where

import Language

import Data.Maybe
import qualified Data.Text as T

validate :: Expression -> Either T.Text Expression
validate expr = case errors of
        [] -> Right expr
        _ -> Left (T.concat errors)
    where
        errors = catMaybes (map ($expr) checks)
        checks = [validate_types]

{- Each checking function has the type
 -
 - Expression -> Maybe T.Text
 -
 - If it finds an error, it returns a Just error; otherwise, Nothing.
 -
 - The validate function just runs all checks and either concatenates all the
 - error messages or passes the expression unharmed on the Right side.
 -}
validate_types :: Expression -> Maybe T.Text
validate_types = const Nothing

