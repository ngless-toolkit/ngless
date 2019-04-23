{- Copyright 2018-2019 NGLess Authors
 - License: MIT
 -}

module StandardModules.Minimap2
    ( loadModule
    ) where

import qualified Data.Text as T
import Data.Default

import NGLess
import Modules
import NGLess.NGLEnvironment

loadModule :: T.Text -> NGLessIO Module
loadModule _ = do
    updateNglEnvironment addMinimap2
    return def
        { modInfo = ModInfo "stdlib.minimap2" "1.0"
        , modCitations = [citation]
        , modFunctions = []
        , runFunction = \_ _ _ -> throwShouldNotOccur "minimap2 has no functions!"
        }
    where
        addMinimap2 e@NGLEnvironment { ngleMappersActive = p } = e { ngleMappersActive = "minimap2":p }
        citation = T.concat
                    ["Li.  Minimap2: pairwise alignment for nucleotide sequences"
                    ," in arXiv (2018) https://arxiv.org/abs/1708.01492"]
