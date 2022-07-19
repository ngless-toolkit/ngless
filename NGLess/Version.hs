{- Copyright 2013-2022 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE CPP #-}
module Version
    ( versionStr
    , versionStrLong
    , dateStr
    , embeddedStr
    ) where

import Data.Version (showVersion)

import Paths_NGLess (version)

versionStr :: String
versionStr = "1.4.2"

versionStrLong :: String
versionStrLong = "1.4.2"

dateStr :: String
dateStr = "July 21 2022"

embeddedStr :: String
#ifdef NO_EMBED_SAMTOOLS_BWA
embeddedStr = "No"
#else
embeddedStr = "Yes"
#endif

