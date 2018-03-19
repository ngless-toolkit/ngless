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
import qualified StandardModules.Mocat as Mocat
import qualified StandardModules.Soap as Soap
import qualified StandardModules.Minimap2 as Minimap2
import qualified StandardModules.Parallel as Parallel
import qualified ExternalModules as Ext

import Modules
import NGLess

loadStdlibModules :: [ModInfo] -> NGLessIO [Module]
loadStdlibModules = mapM loadModules1
    where
        loadModules1 :: ModInfo -> NGLessIO Module
        loadModules1 (ModInfo "example" version) = Example.loadModule version
        loadModules1 (ModInfo "batch" version) = Batch.loadModule version
        loadModules1 (ModInfo "samtools" version) = Samtools.loadModule version
        loadModules1 (ModInfo "mocat" version) = Mocat.loadModule version
        loadModules1 (ModInfo "parallel" version) = Parallel.loadModule version
        loadModules1 (ModInfo "soap" version) = Soap.loadModule version
        loadModules1 (ModInfo "minimap2" version) = Minimap2.loadModule version
        loadModules1 minfo = Ext.loadModule minfo

