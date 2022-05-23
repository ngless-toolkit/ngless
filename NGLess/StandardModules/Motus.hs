{- Copyright 2022 NGLess Authors
 - License: MIT
 -}

module StandardModules.Motus
    ( loadModule
    ) where

import qualified Data.Text as T
import Control.Monad (when)

import Language
import Modules
import Output
import NGLess
import NGLess.NGLEnvironment
import qualified ExternalModules

loadModule :: T.Text -> NGLessIO Module
loadModule ver
    | ver == "1.0" = do
        activeVersion <- ngleVersion <$> nglEnvironment
        when (activeVersion >= NGLVersion 1 4) $
            throwScriptError "motus module is not supported in NGLess 1.4 (it supported motus1 only and that is now very old"
        outputListLno' WarningOutput ["motus module is deprecated\nIt supports motus1 only and that is now very old (please see https://github.com/ngless-toolkit/ngless-contrib/tree/master/motus.ngm)"]
        let info = ModInfo "motus" ver
        ExternalModules.loadModule info
    | otherwise = do
        throwScriptError "To use the motus module for newer versions, you need to download it and import it with 'local import'"
