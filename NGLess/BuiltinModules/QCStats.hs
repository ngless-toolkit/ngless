{- Copyright 2017-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts #-}

module BuiltinModules.QCStats
    ( loadModule
    ) where

import qualified Data.Text as T
import           Data.Default (def)
import Control.Monad.Except
import System.IO

import Language
import FileManagement

import Modules
import Output
import NGLess
import FileOrStream

executeStats :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeStats (NGOSymbol statsType) [] = do
        (fp, h) <- openNGLTempFile "" "qcstats" "tsv"
        liftIO $ hClose h
        case statsType of
            "mapping" -> liftIO $ writeOutputTSV True Nothing (Just fp)
            "fastq" -> liftIO $ writeOutputTSV True (Just fp) Nothing
            _ -> throwScriptError ("Unknown stats type: " ++ T.unpack statsType)
        return $ NGOCounts (File fp)
executeStats arg _ = throwShouldNotOccur ("executeStats called with argument: " ++ show arg)

qcStatsFunction = Function
    { funcName = FuncName "qcstats"
    , funcArgType = Just NGLSymbol
    , funcArgChecks = [ArgCheckSymbol ["fastq", "mapping"]]
    , funcRetType = NGLCounts
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = [FunctionCheckReturnAssigned
                   ,FunctionCheckNGLVersionIncompatibleChange (0, 8)]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.stats" "0.6"
    , modFunctions = [qcStatsFunction]
    , runFunction = const executeStats
    }

