{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utils.Utils
    ( lookupWithDefault
    ) where


import Data.Maybe (fromMaybe)

lookupWithDefault :: Eq b => a -> b -> [(b,a)] -> a
lookupWithDefault def key values = fromMaybe def $ lookup key values
