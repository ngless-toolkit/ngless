{- Copyright 2017-2018 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.Assemble
    ( loadModule
    ) where

import qualified Data.Text as T
import           System.FilePath ((</>))
import           Control.Monad.Except (liftIO)
import           Data.Default (def)
import           GHC.Conc (getNumCapabilities)

import Language
import Configuration
import FileManagement (createTempDir, megahitBin)
import Modules
import Output
import NGLess
import Data.FastQ
import Data.FastQ.Utils
import NGLess.NGLEnvironment
import Utils.Process (runProcess)


executeAssemble :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeAssemble "assemble" expr kwargs = do
    files <- case expr of
            NGOReadSet _ (ReadSet [] singles) -> do
                FastQFilePath _ f <- concatenateFQs singles
                return ["-r", f]
            NGOReadSet _ (ReadSet pairs []) -> do
                FastQFilePath _ f1 <- concatenateFQs (fst <$> pairs)
                FastQFilePath _ f2 <- concatenateFQs (snd <$> pairs)
                return ["-1", f1, "-2", f2]
            NGOReadSet _ (ReadSet pairs singles) -> do
                FastQFilePath _ f1 <- concatenateFQs (fst <$> pairs)
                FastQFilePath _ f2 <- concatenateFQs (snd <$> pairs)
                FastQFilePath _ f3 <- concatenateFQs singles
                return ["-1", f1, "-2", f2, "-r", f3]
            _ -> throwScriptError ("megahit:assemble first argument should have been readset, got '"++show expr++"'")
    megahitPath <- megahitBin
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    nthreads <- liftIO getNumCapabilities
    extraArgs <- map T.unpack <$> lookupStringListOrScriptErrorDef (return []) "extra megahit arguments" "__extra_megahit_args" kwargs
    (_, tdir) <- createTempDir "ngless-megahit-assembly"
    (_, mhtmpdir) <- createTempDir "ngless-megahit-tmpdir"
    let odir = tdir </> "megahit-output"
        args = files ++
                        ["-o", odir
                        ,"--num-cpu-threads", show nthreads
                        ,"--tmp-dir", mhtmpdir
                        ] ++ ["--keep-tmp-files" | keepTempFiles] ++ extraArgs
    outputListLno' DebugOutput ["Calling megahit: ", megahitPath, " ", unwords args]
    runProcess megahitPath args (return ()) (Left ())
    return $! NGOFilename (odir </> "final.contigs.fa")
executeAssemble _ _ _ = throwScriptError "unexpected code path taken [megahit:assemble]"

assembleFunction = Function
    { funcName = FuncName "assemble"
    , funcArgType = Just NGLReadSet
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = [ArgInformation "__extra_megahit_args" False (NGList NGLString) []]
    , funcAllowsAutoComprehension = False
    , funcChecks = [FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.assemble" "0.0"
    , modFunctions = [assembleFunction]
    , runFunction = executeAssemble
    }

