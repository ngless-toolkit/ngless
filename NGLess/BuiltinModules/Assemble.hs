{- Copyright 2017 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.Assemble
    ( loadModule
    ) where

import qualified Data.Text as T
import           System.Process (proc)
import           System.FilePath ((</>))
import           System.Exit
import           Control.Monad.Except (liftIO)
import           Data.Default (def)
import           GHC.Conc (getNumCapabilities)

import Language
import Configuration
import FileManagement (createTempDir, megahitBin)
import Modules
import Output
import NGLess
import NGLess.NGLEnvironment
import Utils.Utils (readProcessErrorWithExitCode)


executeAssemble :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeAssemble "assemble" expr [] = do
    files <- case expr of
            NGOReadSet _ (ReadSet1 _ f) -> return ["-r", f]
            NGOReadSet _ (ReadSet2 _ f1 f2) -> return ["-1", f1, "-2", f2]
            NGOReadSet _ (ReadSet3 _ f1 f2 f3) -> return ["-1", f1, "-2", f2, "-r", f3]
            _ -> throwScriptError ("megahit:assemble first argument should have been readset, got '"++show expr++"'")
    megahitPath <- megahitBin
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    nthreads <- liftIO getNumCapabilities
    (_, tdir) <- createTempDir "ngless-megahit-assembly"
    (_, mhtmpdir) <- createTempDir "ngless-megahit-tmpdir"
    let odir = tdir </> "megahit-output"
        args = files ++
                        ["-o", odir
                        ,"--num-cpu-threads", show nthreads
                        ,"--tmp-dir", mhtmpdir
                        ] ++ ["--keep-tmp-files" | keepTempFiles]
    outputListLno' DebugOutput ["Calling megahit: ", megahitPath, " ", unwords args]
    (errmsg, exitCode) <- liftIO $ readProcessErrorWithExitCode
                                    (proc megahitPath args)
    if null errmsg
        then outputListLno' DebugOutput ["No output from megahit."]
        else outputListLno' InfoOutput ["Message from metahit: ", errmsg]
    case exitCode of
       ExitSuccess -> return $! NGOFilename (odir </> "final.contigs.fa")
       ExitFailure err -> throwSystemError ("Failure on calling megahit, error code: "++show err)
executeAssemble _ _ _ = throwScriptError "unexpected code path taken [megahit:assemble]"

assembleFunction = Function
    { funcName = FuncName "assemble"
    , funcArgType = Just NGLReadSet
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.assemble" "0.0"
    , modFunctions = [assembleFunction]
    , runFunction = executeAssemble
    }

