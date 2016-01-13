{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module StandardModules.NGLStdlib
    ( loadStdlibModules
    ) where


import qualified StandardModules.Example as Example
import qualified StandardModules.Batch as Batch
import qualified StandardModules.Samtools as Samtools
import qualified ExternalModules as Ext

import Utils.Utils
import Modules
import NGLess

loadStdlibModules :: [ModInfo] -> NGLessIO [Module]
loadStdlibModules = mapM (\m -> loadModules1 m >>= passthrough registerModule)

externalModules =
        ["example-cmd"
        ,"motus"
        ,"samtools"
        ]

loadModules1 :: ModInfo -> NGLessIO Module
loadModules1 (ModInfo "example" version) = Example.loadModule version
loadModules1 (ModInfo "batch" version) = Batch.loadModule version
loadModules1 (ModInfo "samtools" version) = Samtools.loadModule version
loadModules1 (ModInfo mname version)
    | mname `elem` externalModules = Ext.loadModule mname version
loadModules1 (ModInfo modname _) = throwScriptError ("Could not load module " ++show modname)

