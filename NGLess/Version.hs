{- Copyright 2013-2022 NGLess Authors
 - License: MIT
 -}
module Version
    ( versionStr
    , versionStrLong
    , dateStr
    ) where

import Data.Version (showVersion)

import Paths_NGLess (version)

versionStr :: String
versionStr = showVersion version

versionStrLong :: String
versionStrLong = "1.5.0"

dateStr :: String
dateStr = "14 September 2022"

