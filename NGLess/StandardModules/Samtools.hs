{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Samtools
    ( loadModule
    ) where

import Control.Monad.Trans.Resource
import Control.Exception
import Control.Concurrent
import Control.Monad

import qualified Data.Text as T
import qualified Control.Concurrent.Async as A
import           Data.Conduit (($$))
import           Data.List.Extra (snoc)


import System.FilePath
import System.Process
import System.Exit
import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Default

import FileManagement
import Configuration
import FileOrStream
import Transform
import Language
import Modules
import Output
import NGLess

import Utils.Conduit
import Utils.Utils


executeSort :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSort (NGOMappedReadSet name istream rinfo) args = do
    outBam <- lookupBoolOrScriptErrorDef (return False) "samtools_sort" "__output_bam" args
    let oformat = if outBam then "bam" else "sam"
        fname = case istream of
                    File f -> f
                    Stream f _ -> f
    (rk, (newfp, hout)) <- openNGLTempFile' fname "sorted_" ("." ++ oformat)
    (trk, tdirectory) <- createTempDir "samtools_sort_temp"

    numCapabilities <- liftIO getNumCapabilities
    let cmdargs = ["sort", "-@", show numCapabilities, "-O", oformat, "-T", tdirectory </> "samruntmp"]
    samtoolsPath <- samtoolsBin
    outputListLno' TraceOutput ["Calling binary ", samtoolsPath, " with args: ", unwords cmdargs]
    (err, exitCode) <- case istream of
        File fpsam -> do
            let cp = (proc samtoolsPath (snoc cmdargs fpsam)) { std_out = UseHandle hout }
            liftIO $ readProcessErrorWithExitCode cp
        Stream _ istream' -> do
            let cp = (proc samtoolsPath cmdargs) { std_in = CreatePipe, std_out = UseHandle hout, std_err = CreatePipe }
            (Just pipe_out, Nothing, Just herr, jHandle) <- liftIO $ createProcess cp
            samP <- liftIO . A.async $ waitForProcess jHandle
            err <- liftIO . A.async $ do
                        -- In a separate thread, consume all the error input.
                        -- the same pattern is used in the implementation of
                        -- readProcessWithErrorCode (which cannot be used here
                        -- as we want control over stdin/stdout)
                        err <- hGetContents herr
                        void (evaluate (length err))
                        hClose herr
                        return err
            istream' $$ byteLineSinkHandle pipe_out
            liftIO $ do
                hClose pipe_out
                A.waitBoth err samP
    outputListLno' DebugOutput ["Samtools err output: ", err]
    liftIO $ hClose hout
    release trk
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done samtools sort"]
            return (NGOMappedReadSet name (File newfp) rinfo)
        ExitFailure code -> do
            release rk
            throwSystemError $ concat ["Failed samtools sort\nCommand line was::\n\t",
                            samtoolsPath, " with args: ", unwords cmdargs,
                            "\nexit code was ", show code, "."]
executeSort _ _ = throwScriptError "Unexpected arguments for samtools_sort function"

sortOFormat :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
sortOFormat = return . sortOFormat'

-- If samtools_sort is called and its return value is only used in a write()
-- call which is in BAM format, then we should immediately use BAM as the
-- output format.
sortOFormat' :: [(Int, Expression)] -> [(Int, Expression)]
sortOFormat' [] = []
sortOFormat' ((lno,e):es) = (lno,e'):sortOFormat' es
    where
        e' = case e of
            Assignment v (FunctionCall fname@(FuncName "samtools_sort") expr args Nothing)
                | outputBam v es -> Assignment v (FunctionCall fname expr ((Variable "__output_bam", ConstBool True):args) Nothing)
            _ -> e
        outputBam _ [] = False
        outputBam v ((_,c):rest) = case c of
            FunctionCall (FuncName "write") (Lookup _ v') args Nothing
                | v == v' -> isOBam args && not (isVarUsed v rest)
            _
                | isVarUsed1 v c -> False
                | otherwise -> outputBam v rest
        -- The rules are
        --  1. if a format argument is present, it takes priority;
        --     else, infer from ofile argument
        --  2. In doubt, False is the safe option
        isOBam args = case oFormat args of
            Just "bam" -> True
            Just _ -> False
            Nothing -> case lookup (Variable "ofile") args of
                Nothing -> False
                Just oname -> stringWillEndWith oname ".bam" == Just True
        oFormat :: [(Variable, Expression)] -> Maybe String
        oFormat args = case lookup (Variable "format") args of
            Just (ConstSymbol "bam") -> Just "bam"
            Just _ -> Just "?"
            Nothing -> Nothing

        stringWillEndWith :: Expression -> T.Text -> Maybe Bool
        stringWillEndWith (ConstStr b) post
            | T.length b >= T.length post = Just $ T.takeEnd (T.length post) b == post
            | otherwise = Nothing
        stringWillEndWith (BinaryOp BOpAdd _ left) post = stringWillEndWith left post
        stringWillEndWith _ _ = Nothing




samtools_sort_function = Function
    { funcName = FuncName "samtools_sort"
    , funcArgType = Just NGLMappedReadSet
    , funcArgChecks = []
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
        , modTransform = sortOFormat
        , runFunction = const executeSort
        }
    where
        citation = T.concat
            ["'The Sequence Alignment/Map format and SAMtools' "
            ,"by Heng Li, Bob Handsaker, Alec Wysoker, Tim Fennell, Jue Ruan, Nils Homer, Gabor Marth, Goncalo Abecasis, Richard Durbin, and 1000 Genome Project Data Processing Subgroup "
            ,"in Bioinformatics (2009) 25 (16): 2078-2079 (2009) DOI:10.1093/bioinformatics/btp352"]
