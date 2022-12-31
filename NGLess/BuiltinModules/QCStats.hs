{- Copyright 2017-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE CPP, FlexibleContexts #-}

module BuiltinModules.QCStats
    ( loadModule
#ifdef IS_BUILDING_TEST
    , pureMod
#endif
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
            "mapping" -> writeOutputTSV True Nothing (Just fp)
            "fastq" -> writeOutputTSV True (Just fp) Nothing
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
                   ,FunctionCheckNGLVersionIncompatibleChange (0, 8) ""]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return pureMod

pureMod :: Module
pureMod = def
    { modInfo = ModInfo "builtin.stats" "0.6"
    , modFunctions = [qcStatsFunction]
    , runFunction = const executeStats
    }

