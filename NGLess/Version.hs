{- Copyright 2013-2020 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, CPP #-}
module Version
    ( versionStr
    , compilationDateStr
    , dateStr
    , embeddedStr
    , gitHashStr
    ) where

import Development.GitRev (gitHash)
import Data.Version (showVersion)

import Paths_NGLess (version)

versionStr :: String
versionStr = showVersion version

dateStr :: String
dateStr = "January 25 2020"

gitHashStr :: String
gitHashStr = $(gitHash)

embeddedStr :: String
#ifdef NO_EMBED_SAMTOOLS_BWA
embeddedStr = "No"
#else
embeddedStr = "Yes"
#endif

compilationDateStr :: String
compilationDateStr = __DATE__
