{- Copyright 2017-2022 NGLess Authors
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
import           Control.Monad.Trans.Resource (release)
import Data.List.NonEmpty qualified as NE

import Language
import Configuration
import FileManagement (ensureCompressionIsOneOf, Compression(..), createTempDir, megahitBin)
import Modules
import Output
import NGLess
import Data.FastQ
import Data.FastQ.Utils
import NGLess.NGLEnvironment
import Utils.Process (runProcess)


maybeRecompress :: FastQFilePath -> NGLessIO FilePath
maybeRecompress (FastQFilePath _ fp) = ensureCompressionIsOneOf (NoCompression `NE.cons` NE.singleton GzipCompression) fp

executeAssemble :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeAssemble "assemble" expr kwargs = do
    (pairs, singles) <- case expr of
        NGOReadSet _ (ReadSet pairs singles) -> return (pairs, singles)
        _ -> throwScriptError ("megahit:assemble first argument should have been readset, got '"++show expr++"'")
    pairedArgs <- case NE.nonEmpty pairs of
        Nothing -> return []
        Just pairs' -> do
            f1 <- maybeRecompress =<< concatenateFQs (fst <$> pairs')
            f2 <- maybeRecompress =<< concatenateFQs (snd <$> pairs')
            return ["-1", f1, "-2", f2]
    singlesArgs <- case NE.nonEmpty singles of
        Nothing -> return []
        Just singles' -> do
            f <- maybeRecompress =<< concatenateFQs singles'
            return ["-r", f]
    megahitPath <- megahitBin
    keepTempFiles <- nConfKeepTemporaryFiles <$> nglConfiguration
    nthreads <- liftIO getNumCapabilities
    extraArgs <- map T.unpack <$> lookupStringListOrScriptErrorDef (return []) "extra megahit arguments" "__extra_megahit_args" kwargs
    (_, tdir) <- createTempDir "ngless-megahit-assembly"
    (mt, mhtmpdir) <- createTempDir "ngless-megahit-tmpdir"
    let odir = tdir </> "megahit-output"
        args = pairedArgs ++ singlesArgs ++
                        ["-o", odir
                        ,"--num-cpu-threads", show nthreads
                        ,"--tmp-dir", mhtmpdir
                        ] ++ ["--keep-tmp-files" | keepTempFiles] ++ extraArgs
    outputListLno' DebugOutput ["Calling megahit: ", megahitPath, " ", unwords args]
    runProcess megahitPath args (return ()) (Left ())
    release mt
    return $! NGOSequenceSet (odir </> "final.contigs.fa")
executeAssemble _ _ _ = throwScriptError "unexpected code path taken [megahit:assemble]"

assembleFunction = Function
    { funcName = FuncName "assemble"
    , funcArgType = Just NGLReadSet
    , funcArgChecks = []
    , funcRetType = NGLSequenceSet
    , funcKwArgs = [ArgInformation "__extra_megahit_args" False (NGList NGLString) []]
    , funcAllowsAutoComprehension = False
    , funcChecks =
        [FunctionCheckNGLVersionIncompatibleChange (1, 4) "Megahit version was updated which significantly changes results"
        ,FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.assemble" "0.0"
    , modFunctions = [assembleFunction]
    , runFunction = executeAssemble
    }

