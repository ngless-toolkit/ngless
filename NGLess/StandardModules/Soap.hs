{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module StandardModules.Soap
    ( loadModule
    ) where

import qualified Data.Text as T
import Data.Default

import NGLess
import Modules
import NGLess.NGLEnvironment

loadModule :: T.Text -> NGLessIO Module
loadModule _ = do
    -- TODO: Add check for SOAP binaries in PATH
    updateNglEnvironment addSoap
    return def
        { modInfo = ModInfo "stdlib.soap" "1.0"
        , modCitations = [citation]
        , modFunctions = []
        , runFunction = \_ _ _ -> throwShouldNotOccur "soap has no functions!"
        }
    where
        addSoap e@NGLEnvironment { ngleMappersActive = p } = e { ngleMappersActive = "soap":p }
        citation = T.concat
                    ["Li, Ruiqiang, Yingrui Li, Karsten Kristiansen, and Jun Wang. \n"
                    ,"\"SOAP: short oligonucleotide alignment program.\"\n"
                    ,"Bioinformatics 24, no. 5 (2008): 713-714."]
