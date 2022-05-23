{- Copyright 2013-2022 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE CPP #-}
module Version
    ( versionStr
    , dateStr
    , embeddedStr
    ) where

import Data.Version (showVersion)

import Paths_NGLess (version)

versionStr :: String
versionStr = showVersion version

dateStr :: String
dateStr = "Unreleased"

embeddedStr :: String
#ifdef NO_EMBED_SAMTOOLS_BWA
embeddedStr = "No"
#else
embeddedStr = "Yes"
#endif

