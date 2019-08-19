{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module StandardModules.Samtools
    ( loadModule
    ) where

import Control.Monad.Trans.Resource
import Control.Exception
import Control.Concurrent
import Control.Monad

import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Text as T
import qualified Control.Concurrent.Async as A
import           Data.List (uncons, tails)
import           Data.Maybe (mapMaybe)


import System.FilePath
import System.Process
import System.Exit
import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Semigroup ((<>))

import FileManagement
import FileOrStream
import Transform
import Language
import Modules
import Output
import NGLess

import Utils.Conduit (byteLineVSinkHandle)
import Utils.Utils (passthrough)


headtails :: [a] -> [(a,[a])]
headtails = mapMaybe uncons . tails

-- This function is necessary because we cannot call readProcessWithErrorCode directly (see comment below)
callSamtools :: FileOrStream -> [String] -> Handle -> NGLessIO ExitCode
callSamtools istream cmdargs hout = do
    samtoolsPath <- samtoolsBin
    outputListLno' TraceOutput ["Calling binary ", samtoolsPath, " with args: ", unwords cmdargs]
    let (_, istream') = asSamStream istream
        cp = (proc samtoolsPath cmdargs) { std_in = CreatePipe, std_out = UseHandle hout, std_err = CreatePipe }
    (Just pipe_out, Nothing, Just herr, jHandle) <- liftIO $ createProcess cp
    samP <- liftIO . A.async $ waitForProcess jHandle
    errP <- liftIO . A.async $ do
                -- In a separate thread, consume all the error input.
                -- the same pattern is used in the implementation of
                -- readProcessWithErrorCode (which cannot be used here
                -- as we want control over stdin/stdout)
                err <- hGetContents herr
                void (evaluate (length err))
                hClose herr
                return err
    C.runConduit $ istream' .| byteLineVSinkHandle pipe_out
    (err,exitCode) <- liftIO $ do
        hClose pipe_out
        A.waitBoth errP samP
    outputListLno' DebugOutput ["Samtools err output: ", err]
    liftIO $ hClose hout
    return exitCode

executeSort :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSort (NGOMappedReadSet name istream rinfo) args = do
    outBam <- lookupBoolOrScriptErrorDef (return False) "samtools_sort" "__output_bam" args
    sortByName <- lookupSymbolOrScriptErrorDef (return "coordinate") "arguments to samtools_sort" "by" args >>= \case
        "coordinate" -> return False
        "name" -> return True
        other -> throwShouldNotOccur ("Check failed. No samtool_sort option: " ++ T.unpack other)
    let oformat = if outBam then "bam" else "sam"
        fname = case istream of
                    File f -> f
                    Stream _ f _ -> f
    (rk, (newfp, hout)) <- openNGLTempFile' fname "sorted_" oformat
    (trk, tdirectory) <- createTempDir "samtools_sort_temp"

    numCapabilities <- liftIO getNumCapabilities
    let cmdargs = ["sort"] <> ["-n" | sortByName] <> ["-@", show numCapabilities, "-O", oformat, "-T", tdirectory </> "samruntmp"]
    exitCode <- callSamtools istream cmdargs hout
    release trk
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done samtools sort"]
            return (NGOMappedReadSet name (File newfp) rinfo)
        ExitFailure code -> do
            release rk
            samtoolsPath <- samtoolsBin
            throwSystemError $ concat ["Failed samtools sort\n",
                            "Executable used::\t", samtoolsPath,"\n",
                            "Command line was::\n\t", unwords cmdargs, "\n",
                            "Samtools exit code was ", show code, "."]
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


checkUnique :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
checkUnique = passthrough $ \sc ->
        forM_ (headtails $ reverse sc) $ \((lno, e), rest) -> case e of
            Assignment _ (FunctionCall (FuncName "select") (Lookup _ v) kwargs Nothing)
                        | selectsUnique kwargs -> checkSorted lno v (map snd rest)
            _ -> return ()
    where
        checkSorted _ _ [] = return ()
        checkSorted lno v (r:rs) = case r of
            Assignment v' expr
                | v == v' -> case expr of
                    FunctionCall (FuncName "samtools_sort") _ _ _ ->
                            throwScriptError ("Cannot select unique reads from a sorted mappedreadset (at line " ++ show lno ++ ").\n\tConsider selecting, *then* sorting.")
                    Lookup _ v'' -> checkSorted lno v'' rs
                    _ -> return ()
            _ -> checkSorted lno v rs
        selectsUnique :: [(Variable, Expression)] -> Bool
        selectsUnique = any $ \(Variable k,v) -> k == "keep_if" && hasUnique v
        hasUnique (ConstSymbol "unique") = True
        hasUnique (ListExpression vs) = any hasUnique vs
        hasUnique _ = False

samtools_sort_function = Function
    { funcName = FuncName "samtools_sort"
    , funcArgType = Just NGLMappedReadSet
    , funcArgChecks = []
    , funcRetType = NGLMappedReadSet
    , funcKwArgs = [ArgInformation "by" False NGLSymbol [ArgCheckSymbol ["coordinate", "name"]]]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }


executeView :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeView (NGOMappedReadSet name istream rinfo) args = do
    outBam <- lookupBoolOrScriptErrorDef (return False) "samtools_view" "__output_bam" args
    bedFile <- lookupStringOrScriptError "coordinates in BED format" "bed_file" args

    let oformat = if outBam then "bam" else "sam"
        fname = case istream of
                    File f -> f
                    Stream _ f _ -> f

    (rk, (newfp, hout)) <- openNGLTempFile' fname "subset_" oformat

    numCapabilities <- liftIO getNumCapabilities
    let cmdargs = ["view", "-h", "-@", show numCapabilities, "-O", oformat, "-L", T.unpack bedFile]
    exitCode <- callSamtools istream cmdargs hout
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done samtools view"]
            return (NGOMappedReadSet name (File newfp) rinfo)
        ExitFailure code -> do
            release rk
            samtoolsPath <- samtoolsBin
            throwSystemError $ concat ["Failed samtools view\n",
                            "Executable used::\t", samtoolsPath,"\n",
                            "Command line was::\n\t", unwords cmdargs, "\n",
                            "Samtools exit code was ", show code, "."]
executeView _ _ = throwScriptError "Unexpected arguments for samtools_view function"

samtools_view_function = Function
    { funcName = FuncName "samtools_view"
    , funcArgType = Just NGLMappedReadSet
    , funcArgChecks = []
    , funcRetType = NGLMappedReadSet
    , funcKwArgs = [ArgInformation "bed_file" True NGLString [ArgCheckFileReadable]]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

loadModule :: T.Text -> NGLessIO Module
loadModule v
    | v == "0.0" = return def
                   { modInfo = ModInfo "stdlib.samtools" "0.0"
                   , modCitations = [citation]
                   , modFunctions = [samtools_sort_function]
                   , modTransform = sortOFormat >=> checkUnique
                   , runFunction = \case
                                   "samtools_sort" -> executeSort
                                   other -> \_ _ -> throwShouldNotOccur ("samtools runction called with wrong arguments: " ++ show other)
                   }
    | v `elem` ["1.0", "0.1"] = return def
                   { modInfo = ModInfo "stdlib.samtools" "1.0"
                   , modCitations = [citation]
                   , modFunctions = [samtools_sort_function, samtools_view_function]
                   , modTransform = sortOFormat >=> checkUnique
                   , runFunction = \case
                                   "samtools_sort" -> executeSort
                                   "samtools_view" -> executeView
                                   other -> \_ _ -> throwShouldNotOccur ("samtools runction called with wrong arguments: " ++ show other)
                                   }
    | otherwise = throwScriptError ("samtools module version " ++ T.unpack v ++ " doesn't exist")
    where
        citation = T.concat
            ["'The Sequence Alignment/Map format and SAMtools' "
            ,"by Heng Li, Bob Handsaker, Alec Wysoker, Tim Fennell, Jue Ruan, Nils Homer, Gabor Marth, Goncalo Abecasis, Richard Durbin, and 1000 Genome Project Data Processing Subgroup "
            ,"in Bioinformatics (2009) 25 (16): 2078-2079 (2009) DOI:10.1093/bioinformatics/btp352"]
