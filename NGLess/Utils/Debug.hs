{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utils.Debug
    ( tracex
    , trace
    ) where


import Debug.Trace

tracex :: Show a => a -> a
tracex x = trace (show x) x
