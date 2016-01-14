{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Samtools
    ( loadModule
    ) where

import System.FilePath.Posix

import qualified Data.Text as T
import Control.Monad.Trans.Resource

import GHC.Conc (numCapabilities)

import System.Process
import System.Exit

import Control.Monad.IO.Class (liftIO)
import Data.Default

import Language
import FileManagement
import Configuration
import Output
import NGLess
import Modules
import Utils.Utils


executeSort :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSort (NGOMappedReadSet name fpsam rinfo) _ = do
    (rk, (newfp, hout)) <- openNGLTempFile' fpsam "sorted_" ".sam"
    (trk, tdirectory) <- createTempDir "samtools_sort_temp"
    let cmdargs = ["sort", "-@", show numCapabilities, "-O", "sam", "-T", tdirectory </> "samruntmp", fpsam]
    samtoolsPath <- samtoolsBin
    outputListLno' TraceOutput ["Calling binary ", samtoolsPath, " with args: ", unwords cmdargs]
    let cp = (proc samtoolsPath cmdargs) { std_out = UseHandle hout }
    (err, exitCode) <- liftIO $ readProcessErrorWithExitCode cp
    outputListLno' DebugOutput ["Samtools err output: ", err]
    release trk
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done samtools sort"]
            return (NGOMappedReadSet name newfp rinfo)
        ExitFailure code -> do
            release rk
            throwSystemError $ concat ["Failed samtools sort\nCommand line was::\n\t",
                            samtoolsPath, " with args: ", unwords cmdargs,
                            "\nexit code was ", show code, "."]
executeSort _ _ = throwScriptError ("Unexpected arguments for samtools_sort function" :: String)


samtools_sort_function = Function
    { funcName = FuncName "samtools_sort"
    , funcArgType = Just NGLMappedReadSet
    , funcRetType = NGLMappedReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }


loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.samtools" "0.0"
        , modCitation = Just citation
        , modFunctions = [samtools_sort_function]
        , runFunction = const executeSort
        }
    where
        citation = T.concat
            ["'The Sequence Alignment/Map format and SAMtools' "
            ,"by Heng Li, Bob Handsaker, Alec Wysoker, Tim Fennell, Jue Ruan, Nils Homer, Gabor Marth, Goncalo Abecasis, Richard Durbin, and 1000 Genome Project Data Processing Subgroup "
            ,"in Bioinformatics (2009) 25 (16): 2078-2079 (2009) DOI:10.1093/bioinformatics/btp352"]
