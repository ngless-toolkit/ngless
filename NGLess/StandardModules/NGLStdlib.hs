{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module StandardModules.NGLStdlib
    ( loadStdlibModules
    ) where


import qualified StandardModules.Example as Example
import Modules

-- loadStdlibModules :: [ModInfo] -> NGLessIO [Module]
loadStdlibModules mods = mapM loadModules1 mods
loadModules1 (ModInfo "example" version) = Example.loadModule version

