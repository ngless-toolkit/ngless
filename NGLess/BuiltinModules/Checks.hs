{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.Checks
    ( checkOFile
    , loadModule
    ) where

import qualified Data.Text as T
import Control.Monad.Except
import Data.Default
import System.Directory
import System.FilePath.Posix (takeDirectory)


import Language

import Modules
import NGLess

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
executeChecks _ _ _ = throwShouldNotOccur "checks called in an unexpected fashion."

oFileCheck = Function
    { funcName = FuncName "__check_ofile"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs = [ArgInformation "lno" True NGLInteger []]
    , funcAllowsAutoComprehension = False
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.checks" "0.0"
    , modFunctions = [oFileCheck]
    , runFunction = executeChecks
    }

