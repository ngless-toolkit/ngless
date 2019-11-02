{- Copyright 2017-2019 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.ORFFind
    ( loadModule
    ) where

import qualified Data.Text as T
import           System.IO (hClose)
import           Control.Monad.Except (liftIO)
import           Data.Default (def)

import Language
import Modules
import Output
import NGLess
import FileOrStream
import FileManagement (prodigalBin, openNGLTempFile)
import Utils.Process (runProcess)
import Utils.Utils (fmapMaybeM)

executeORFFind :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeORFFind "orf_find" expr kwargs = do
    input <- case expr of
            NGOSequenceSet f -> return f
            NGOFilename f -> return $ File f
            NGOString f -> return $ File (T.unpack f)
            _ -> throwScriptError ("orf_find first argument should have been sequenceset, got '"++show expr++"'")
    isMetagenome <- lookupBoolOrScriptError "orf_find" "is_metagenome" kwargs
    coordsOut <- fmapMaybeM (stringOrTypeError "coords_out argument for orf_find") $ lookup "coords_out" kwargs
    aaOut <- fmapMaybeM (stringOrTypeError "prots_out argument for orf_find") $ lookup "prots_out" kwargs
    includeFragments <- lookupBoolOrScriptErrorDef (return True) "include_fragments arg for orf_find" "include_fragments" kwargs
    prodigalPath <- prodigalBin
    fp <- asFile input
    (dnaout, h) <- openNGLTempFile fp "gene_predict" "fna"
    liftIO $ hClose h
    let args = ["-i", fp,
                "-d", dnaout]
                ++ (if isMetagenome
                            then ["-p", "meta"]
                            else [])
                ++ (case coordsOut of
                        Nothing -> ["-o", "/dev/null"]
                        Just c -> ["-o", T.unpack c, "-f", "gff"])
                ++ (case aaOut of
                        Nothing -> []
                        Just ao -> ["-a", T.unpack ao])
                ++ ["-c" | not includeFragments]

    outputListLno' DebugOutput ["Calling prodigal: ", prodigalPath, " ", unwords args]
    runProcess prodigalPath args (return ()) (Left ())
    -- return $! NGOSequenceSet (File dnaout)
    return $! NGOFilename dnaout
executeORFFind _ _ _ = throwScriptError "unexpected code path taken [prodigal:orf_find]"

orfFindFunction = Function
    { funcName = FuncName "orf_find"
    -- , funcArgType = Just NGLSequenceSet
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLFilename
    , funcKwArgs =
            [ArgInformation "is_metagenome" True NGLBool []
            ,ArgInformation "include_fragments" False NGLBool [ArgCheckMinVersion (1,1)]
            ,ArgInformation "coords_out" False NGLString [ArgCheckFileWritable]
            ,ArgInformation "prots_out" False NGLString [ArgCheckFileWritable]
            ]
    , funcAllowsAutoComprehension = False
    , funcChecks = [FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.orffind" "0.6"
    , modFunctions = [orfFindFunction]
    , runFunction = executeORFFind
    }

