{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}

module Interpretation.Map
    ( executeMap
    , executeMapStats
    , _samStats
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as C
import           Data.Conduit (($$), (=$=))

import GHC.Conc     (getNumCapabilities)

import System.Process
import System.IO
import System.Exit

import Language
import FileManagement
import ReferenceDatabases
import Configuration
import Output
import NGLess

import Data.Sam
import Utils.Utils
import Utils.Bwa
import Utils.LockFile
import Utils.Conduit
import Utils.Samtools (samBamConduit)

data ReferenceInfo = PackagedReference T.Text | FaFile FilePath

lookupReference :: KwArgsValues -> NGLessIO ReferenceInfo
lookupReference args = do
    let reference = lookup "reference" args
        fafile = lookup "fafile" args
    case (reference, fafile) of
        (Nothing, Nothing) -> throwScriptError "Either reference or fafile must be passed"
        (Just _, Just _) -> throwScriptError "Reference and fafile cannot be used simmultaneously"
        (Just r, Nothing) -> PackagedReference <$> stringOrTypeError "reference in map argument" r
        (Nothing, Just fa) -> (FaFile . T.unpack) <$> stringOrTypeError "fafile in map argument" fa

ensureIndexExists :: FilePath -> NGLessIO FilePath
ensureIndexExists refPath = do
    hasIndex <- hasValidIndex refPath
    if hasIndex
        then outputListLno' DebugOutput ["Index for ", refPath, " already exists."]
        else withLockFile LockParameters
                            { lockFname = refPath ++ ".ngless-index.lock"
                            , maxAge = hoursToDiffTime 36
                            , whenExistsStrategy = IfLockedRetry { nrLockRetries = 37*60, timeBetweenRetries = 60 }
                            } $ do
                -- recheck if index exists with the lock in place
                -- it may have been created in the meanwhile (especially if we slept waiting for the lock)
                hasIndex' <- hasValidIndex refPath
                unless hasIndex' $
                    createIndex refPath
    return refPath


mergeSams name sam0 sam1 = do
    (newfp, hout) <- openNGLTempFile name "mapped_concat_" ".sam"
    CB.sourceFile sam0 $$ CB.sinkHandle hout
    CB.sourceFile sam1
        =$= CB.lines
        =$= CL.filter (\line -> not (B.null line) &&  B8.head line /= '@')
        =$= C.unlinesAscii
        $$ CB.sinkHandle hout
    liftIO $ hClose hout
    return newfp

hoursToDiffTime h = fromInteger (h * 3600)

mapToReference :: FilePath -> [FilePath] -> [String] -> NGLessIO (FilePath, (Int, Int, Int))
mapToReference refIndex [fp1,fp2, fp3] extraArgs = do
    (out0,(t0,a0,u0)) <- mapToReference refIndex [fp1, fp2] extraArgs
    (out1,(t1,a1,u1)) <- mapToReference refIndex [fp3] extraArgs
    out <- mergeSams refIndex out0 out1
    return (out, (t0+t1,a0+a1, u0+u1))

mapToReference refIndex fps extraArgs = do
    (rk, (newfp, hout)) <- openNGLTempFile' refIndex "mapped_" ".sam"
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    outputListLno' DebugOutput ["Write .sam file to: ", newfp]
    bwaPath <- bwaBin
    numCapabilities <- liftIO getNumCapabilities
    let cmdargs =  concat [["mem", "-t", show numCapabilities, refIndex], extraArgs, fps]
    outputListLno' TraceOutput ["Calling binary ", bwaPath, " with args: ", unwords cmdargs]
    let cp = proc bwaPath cmdargs
    (exitCode, ((),statsE), err) <- liftIO $
            CP.sourceProcessWithStreams cp
                (return ()) -- stdin
                (C.toConsumer (zipSink2 --stdout
                    (C.sinkHandle hout)
                    samStatsC))
                CL.consume -- stderr
    liftIO $ hClose hout
    stats <- runNGLess statsE
    outputListLno' DebugOutput ["BWA info: ", BL8.unpack $ BL8.fromChunks err]
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done mapping to ", refIndex]
            return (newfp, stats)
        ExitFailure code -> do
            release rk
            throwSystemError $ concat (["Failed mapping\nCommand line was::\n\t",
                            bwaPath, " mem -t ", show numCapabilities, " '", refIndex, "' '"] ++ fps ++ ["'\n",
                            "Bwa error code was ", show code, "."])

interpretMapOp :: ReferenceInfo -> T.Text -> [FilePath] -> [String] -> NGLessIO NGLessObject
interpretMapOp ref name fps extraArgs = do
    (ref', defGen') <- indexReference ref
    (samPath', (total, aligned, unique)) <- mapToReference ref' fps extraArgs
    outputMapStatistics samPath' ref' total aligned unique
    return $ NGOMappedReadSet name samPath' defGen'
    where
        indexReference :: ReferenceInfo -> NGLessIO (FilePath, Maybe T.Text)
        indexReference (FaFile fa) = (,Nothing) <$> ensureIndexExists fa
        indexReference (PackagedReference r) = do
            ReferenceFilePaths fafile _ _ <- ensureDataPresent r
            case fafile of
                Just fp -> (, Just r) <$> ensureIndexExists fp
                Nothing -> throwScriptError ("Could not find reference '" ++ T.unpack r ++ "'.")

_samStats :: FilePath -> NGLessIO (Int, Int, Int)
_samStats fname = samBamConduit fname $$ samStatsC >>= runNGLess

samStatsC :: (MonadIO m) => C.Sink B.ByteString m (NGLess (Int, Int, Int))
samStatsC = do
    let add1if !v True = v+1
        add1if !v False = v
        summarize _ [] = error "This is a bug in ngless"
        summarize (!t, !al, !u) g = let
                    aligned = any isAligned g
                    sameRName = allSame (samRName <$> g)
                    unique = aligned && sameRName
            in
                (t + 1
                ,add1if al aligned
                ,add1if  u unique
                )
    runExceptC $
        linesC
        =$= readSamGroupsC
        =$= CL.fold summarize (0, 0, 0)

-- | this is copied from runErrorC, using ExceptT as we do not want to have to
-- make `e` be of class `Error`.
runExceptC :: (Monad m) => C.Sink i (ExceptT e m) r -> C.Sink i m (Either e r)
runExceptC (C.ConduitM c0) =
    C.ConduitM $ \rest ->
        let go (C.Done r) = rest (Right r)
            go (C.PipeM mp) = C.PipeM $ do
                eres <- runExceptT mp
                return $! case eres of
                    Left e -> rest $ Left e
                    Right p -> go p
            go (C.Leftover p i) = C.Leftover (go p) i
            go (C.HaveOutput p f o) = C.HaveOutput (go p) (runExceptT f >> return ()) o
            go (C.NeedInput x y) = C.NeedInput (go . x) (go . y)
         in go (c0 C.Done)

executeMap :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeMap fps args = do
    ref <- lookupReference args
    extraArgs <- map T.unpack <$> lookupStringListOrScriptErrorDef (return []) "extra bwa arguments" "__extra_bwa_args" args
    let executeMap' (NGOList es) = NGOList <$> forM es executeMap'
        executeMap' (NGOReadSet name (ReadSet1 _enc file))   = interpretMapOp ref name [file] extraArgs
        executeMap' (NGOReadSet name (ReadSet2 _enc fp1 fp2)) = interpretMapOp ref name [fp1,fp2] extraArgs
        executeMap' (NGOReadSet name (ReadSet3 _enc fp1 fp2 fp3)) = interpretMapOp ref name [fp1,fp2,fp3] extraArgs
        executeMap' v = throwShouldNotOccur ("map expects ReadSet, got " ++ show v ++ "")
    executeMap' fps

executeMapStats :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeMapStats (NGOMappedReadSet name sampf _) _ = do
    outputListLno' TraceOutput ["Computing mapstats on ", sampf]
    (t,al,u) <- _samStats sampf
    (countfp, hout) <- openNGLTempFile sampf "sam_stats_" ".stats"
    liftIO . hPutStr hout . concat $
        [     "\t",  T.unpack name, "\n"
        ,"total\t",   show  t, "\n"
        ,"aligned\t", show al, "\n"
        ,"unique\t",  show  u, "\n"
        ]
    liftIO $ hClose hout
    return $! NGOCounts countfp
executeMapStats other _ = throwScriptError ("Wrong argument for mapstats: "++show other)
