{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Parallel
    ( loadModule
    ) where

import qualified Data.Text as T
import System.FilePath.Posix
import Control.Monad.IO.Class (liftIO)
import Data.Default
import System.Directory (createDirectoryIfMissing)

import Output
import NGLess
import Modules
import Language
import Utils.LockFile

executeLock1 (NGOList entries) _  = do
    entries' <- mapM (stringOrTypeError "lock1") entries
    let lockdir = "ngless-locks"
    liftIO $ createDirectoryIfMissing True lockdir
    tryLock lockdir entries'
executeLock1 arg _ = throwScriptError ("Wrong argument for lock1 (expected a list of strings, got `" ++ show arg ++ "`")


tryLock _ [] = do
   outputListLno' InfoOutput ["Could get a lock for any file."]
   throwGenericError ("Could not obtain any lock" :: String)
tryLock basedir (x:xs) =
        acquireLock (basedir </> (T.unpack x) ++ ".lock") >>= \case
            Nothing -> tryLock basedir xs
            Just _ -> return $! NGOString x
    
lock1 = Function
    { funcName = FuncName "lock1"
    , funcArgType = Just (NGList NGLString)
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }


loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.parallel" "0.0"
        , modFunctions = [lock1]
        , runFunction = const executeLock1
        }
