{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

module Utils.Debug
    ( tracex
    , trace
    ) where


import Debug.Trace

-- | Show value when evaluated, then return it
tracex :: Show a => a -> a
tracex x = trace (show x) x
{-# WARNING tracex "Using tracex [remove before committing]" #-}
