{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Interpret
    ( interpret
    ) where

import Language

interpret :: Expression -> IO ()
interpret = putStrLn . show
