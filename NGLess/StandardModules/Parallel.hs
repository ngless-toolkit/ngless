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
import Control.Monad.Trans.Resource
import System.IO
import Data.Default
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Output
import NGLess
import Modules
import Language
import Hooks
import Utils.LockFile

executeLock1 (NGOList entries) _  = do
    entries' <- mapM (stringOrTypeError "lock1") entries
    let lockdir = "ngless-locks"
    liftIO $ createDirectoryIfMissing True lockdir
    (e,rk) <- getLock lockdir entries'
    registerHook FinishOkHook $ do
        let receiptfile = lockdir </> (T.unpack e) ++ ".finished"
        liftIO $ withFile receiptfile WriteMode $ \h ->
            hPutStrLn h (concat ["Finished ", T.unpack e])
        release rk
    return $! NGOString e

executeLock1 arg _ = throwScriptError ("Wrong argument for lock1 (expected a list of strings, got `" ++ show arg ++ "`")


getLock _ [] = do
   outputListLno' InfoOutput ["Could get a lock for any file."]
   throwGenericError ("Could not obtain any lock" :: String)
getLock basedir (x:xs) = do
    let fname = T.unpack x
    finished <- liftIO $ doesFileExist (basedir </> fname ++ ".finished")
    if finished
        then getLock basedir xs
        else acquireLock (basedir </> fname ++ ".lock") >>= \case
            Nothing -> getLock basedir xs
            Just rk -> return (x,rk)

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
