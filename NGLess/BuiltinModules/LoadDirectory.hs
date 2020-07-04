{-# LANGUAGE MultiWayIf #-}
{- Copyright 2016-2020 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.LoadDirectory
    ( loadModule
    , executeLoad
    ) where

import qualified Data.Text as T
import           Control.Monad.Extra (unlessM)
import           System.Directory (doesDirectoryExist)
import System.FilePath
import System.FilePath.Glob
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Maybe
import Data.Default (def)
import Data.List (sort, nub, isInfixOf, isSuffixOf)


import Output
import NGLess
import Modules
import Language
import Utils.Utils (dropEnd)
import Interpretation.FastQ (executeGroup, executePaired, executeFastq)

exts :: [FilePath]
exts = do
    fq <- ["fq", "fastq"]
    comp <- ["", ".gz", ".bz2"]
    return $! fq ++ comp

pairedEnds :: [(String, String)]
pairedEnds = do
    end <- exts
    (s1,s2) <- [(".1", ".2") ,("_1", "_2"), ("_F", "_R")]
    return (s1 ++ "." ++ end, s2 ++ "." ++ end)

buildSingle m1
    | "pair.1" `isInfixOf` m1 = T.unpack $ T.replace "pair.1" "single" (T.pack m1)
    | "pair.2" `isInfixOf` m1 = T.unpack $ T.replace "pair.2" "single" (T.pack m1)
    | otherwise = "MARKER_FOR_FILE_WHICH_DOES_NOT_EXIST"

mocatSamplePaired :: [FilePath] -> T.Text -> Bool -> NGLessIO [NGLessObject]
mocatSamplePaired fqfiles encoding doQC = do
    let passthru = [("__perform_qc", NGOBool doQC), ("encoding", NGOSymbol encoding)]
        match1 :: FilePath -> Maybe (FilePath, FilePath)
        match1 fp = listToMaybe . flip mapMaybe pairedEnds $ \(p1,p2) -> if
                        | (isSuffixOf p1 fp) -> Just (fp, dropEnd (length p1) fp ++ p2)
                        | (isSuffixOf p2 fp) -> Just (dropEnd (length p2) fp ++ p1, fp)
                        | otherwise -> Nothing
        -- match1 returns repeated entries if both pair.1 and pair.2 exist, `nub` removes duplicate records
        matched1 = nub $ mapMaybe match1 fqfiles
        encodeStr = NGOString . T.pack
    (exps,used) <- fmap unzip $ forM matched1 $ \(m1,m2) -> do
        let singles = buildSingle m1
        unless (m1 `elem` fqfiles) $ throwDataError ("Cannot find match for file: " ++ m2)
        unless (m2 `elem` fqfiles) $ throwDataError ("Cannot find match for file: " ++ m1)
        let singlesArgs
                | singles `elem` fqfiles = ("singles", encodeStr singles):passthru
                | otherwise = passthru
        outputListLno' InfoOutput [
                        "load_fastq_directory found paired-end sample '",
                        m1, "' - '", m2,
                        if singles `elem` fqfiles
                            then "' with singles file '" ++ singles ++ "'"
                            else "'"]
        expr <- executePaired (encodeStr m1) (("second", encodeStr m2):singlesArgs)
        let used = [m1, m2, singles]
        return (expr, used)
    let singletonFiles = [f | f <- fqfiles, f `notElem` concat used]
    singletons <- forM singletonFiles $ \f -> do
        outputListLno' InfoOutput ["load_fastq_directory found single-end sample '", f, "'"]
        executeFastq (encodeStr f) passthru
    return $ singletons ++ exps


executeLoad :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoad (NGOString samplename) kwargs = do
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "hidden QC argument" "__perform_qc" kwargs
    encoding <- lookupSymbolOrScriptErrorDef (return "auto") "encoding passthru argument" "encoding" kwargs
    outputListLno' TraceOutput ["Executing load_fastq_directory transform"]
    let basedir = T.unpack samplename
    unlessM (liftIO $ doesDirectoryExist basedir) $
        throwDataError ("Attempting to load directory '"++basedir++"', but directory does not exist.")
    fqfiles <- fmap (sort . concat) $ forM exts $ \pat ->
        liftIO $ namesMatching (basedir </> ("*." ++ pat))
    args <- mocatSamplePaired fqfiles encoding qcNeeded
    executeGroup (NGOList args) [("name", NGOString samplename)]
executeLoad _ _ = throwShouldNotOccur "load_fastq_directory got the wrong arguments."


loadFastqDirectory = Function
    { funcName = FuncName "load_fastq_directory"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLReadSet
    , funcKwArgs =
            [ArgInformation "__perform_qc" False NGLBool []
            ,ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
            ]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }


loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "builtin.load_directory" "1.0"
        , modCitations = []
        , modFunctions = [loadFastqDirectory]
        , runFunction = \case
                        "load_fastq_directory" -> executeLoad
                        other -> \_ _ -> throwShouldNotOccur ("mocat execute function called with wrong arguments: " ++ show other)
        }
