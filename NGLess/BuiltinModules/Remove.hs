{- Copyright 2016 NGLess Authors
 - License: MIT
 -}


module BuiltinModules.Remove
    ( loadModule
    ) where

import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Default (def)

import Data.FastQ
import Language
import FileManagement (removeIfTemporary)
import FileOrStream
import Modules
import NGLess


executeRemove :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeRemove "__remove" expr [] = do
    let files = case expr of
            NGOReadSet _ (ReadSet pairs singles) ->
                                (fqpathFilePath . fst <$> pairs)
                                ++ (fqpathFilePath . snd <$> pairs)
                                ++ (fqpathFilePath <$> singles)
            NGOMappedReadSet _ (File f) _ -> [f]
            NGOCounts (File f) -> [f]
            _ -> []
    forM_ files removeIfTemporary
    return NGOVoid
executeRemove _ _ _ = return NGOVoid

removeFunction = Function
    { funcName = FuncName "__remove"
    , funcArgType = Nothing
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.remove" "0.0"
    , modFunctions = [removeFunction]
    , runFunction = executeRemove
    }

