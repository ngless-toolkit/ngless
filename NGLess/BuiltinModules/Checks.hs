{- Copyright 2016-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.Checks
    ( checkOFile
    , loadModule
    ) where

import qualified Data.Text as T
import           Data.Default (def)
import Control.Monad.Except
import System.Directory
import System.FilePath (takeDirectory)


import Language

import Modules
import NGLess
import           Utils.Suggestion (checkFileReadable)

checkOFile :: T.Text -> IO (Maybe T.Text)
checkOFile ofile = do
    let dirname = takeDirectory (T.unpack ofile)
    exists <- doesDirectoryExist dirname
    if not exists
        then return . Just $! T.concat ["File name '", ofile, "' used as output, but directory ", T.pack dirname, " does not exist."]
        else do
            canWrite <- writable <$> getPermissions dirname
            return $! if canWrite
                    then Nothing
                    else Just (T.concat ["write call to file ", ofile, ", but directory ", T.pack dirname, " is not writable."])


executeChecks :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeChecks "__check_ofile" expr args = do
    oname <- stringOrTypeError "o file check" expr
    lno <- lookupIntegerOrScriptError "o file lno" "original_lno" args
    liftIO (checkOFile oname) >>= \case
        Nothing -> return NGOVoid
        Just err -> throwSystemError $! concat [T.unpack err, " (used in line ", show lno, ")."]
executeChecks "__check_ifile" expr args = do
    oname <- stringOrTypeError "input file check" expr
    lno <- lookupIntegerOrScriptError "inputo file lno" "original_lno" args
    liftIO (checkFileReadable $ T.unpack oname) >>= \case
        Nothing -> return NGOVoid
        Just err -> throwSystemError $! concat [T.unpack err, " (used in line ", show lno, ")."]
executeChecks "__check_index_access" (NGOList vs) args = do
    lno <- lookupIntegerOrScriptError "index access check" "original_lno" args
    index1 <- lookupIntegerOrScriptError "index access check" "index1" args
    when (fromInteger index1 >= length vs) $
        throwScriptError (concat ["Index access on line ", show lno, " is invalid.\n Accessing element with index ", show index1,
                    " but list only has ", show (length vs), " elements.",
                    (if fromInteger index1 == length vs
                        then "\nPlease note that NGLess uses 0-based indexing."
                        else "")
                    ])
    return NGOVoid
executeChecks _ _ _ = throwShouldNotOccur "checks called in an unexpected fashion."

indexCheck = Function
    { funcName = FuncName "__check_index_access"
    , funcArgType = Nothing
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs =
                [ArgInformation "original_lno" True NGLInteger []
                ,ArgInformation "index1" True NGLInteger []]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

oFileCheck = Function
    { funcName = FuncName "__check_ofile"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs = [ArgInformation "original_lno" True NGLInteger []]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

iFileCheck = Function
    { funcName = FuncName "__check_ifile"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs = [ArgInformation "original_lno" True NGLInteger []]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.checks" "0.0"
    , modFunctions = [oFileCheck, iFileCheck, indexCheck]
    , runFunction = executeChecks
    }

