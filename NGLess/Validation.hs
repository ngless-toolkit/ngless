{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE OverloadedStrings #-}
module Validation
    ( validate
    ) where

import Language

import Data.Maybe
import qualified Data.Text as T

validate :: Script -> Either T.Text Script
validate expr = case errors of
        [] -> Right expr
        _ -> Left (T.concat errors)
    where
        errors = catMaybes (map ($expr) checks)
        checks =
            [validate_types
            ,validate_version
            ]

{- Each checking function has the type
 -
 - Script -> Maybe T.Text
 -
 - If it finds an error, it returns a Just error; otherwise, Nothing.
 -
 - The validate function just runs all checks and either concatenates all the
 - error messages or passes the script unharmed on the Right side.
 -}
validate_types :: Script -> Maybe T.Text
validate_types = const Nothing

validate_version (Script (major,minor) _)
    | major /= 0 || minor /= 0 = Just (T.concat ["Version ", T.pack (show  major), ".", T.pack (show minor), " is not supported (nly version 0.0 is available)."])
validate_version _ = Nothing

