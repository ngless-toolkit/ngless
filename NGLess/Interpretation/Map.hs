{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Interpretation.Map
    ( executeMap
    , executeMapStats
    , _samStats
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Control.Monad
import           Control.Monad.Except

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as C
import           Data.Conduit (($$), (=$=))
import           Control.Monad.Extra (unlessM)


import System.IO

import Language
import FileManagement
import ReferenceDatabases
import Output
import NGLess
import NGLess.NGLEnvironment

import qualified StandardModules.Mappers.Bwa as Bwa
import qualified StandardModules.Mappers.Soap as Soap

import Data.Sam
import Utils.Utils
import FileOrStream
import Utils.Conduit
import Utils.LockFile
import Utils.Samtools (samBamConduit)

data ReferenceInfo = PackagedReference T.Text | FaFile FilePath

data Mapper = Mapper
    { createIndex :: FilePath -> NGLessIO ()
    , hasValidIndex :: FilePath -> NGLessIO Bool
    , callMapper :: forall a. FilePath -> [FilePath] -> [String] -> C.Consumer B.ByteString IO a -> NGLessIO a
    }

bwa = Mapper Bwa.createIndex Bwa.hasValidIndex Bwa.callMapper
soap = Mapper Soap.createIndex Soap.hasValidIndex Soap.callMapper

getMapper :: NGLessIO Mapper
getMapper = getMapper' . ngleMapperToUse <$> nglEnvironment
    where
        getMapper' "soap" = soap
        getMapper' _ = bwa

ensureIndexExists :: Mapper -> FilePath -> NGLessIO FilePath
ensureIndexExists mapper refPath = do
    hasIndex <- hasValidIndex mapper refPath
    if hasIndex
        then outputListLno' DebugOutput ["Index for ", refPath, " already exists."]
        else withLockFile LockParameters
                            { lockFname = refPath ++ ".ngless-index.lock"
                            , maxAge = hoursToDiffTime 36
                            , whenExistsStrategy = IfLockedRetry { nrLockRetries = 37*60, timeBetweenRetries = 60 }
                            } $
                -- recheck if index exists with the lock in place
                -- it may have been created in the meanwhile (especially if we slept waiting for the lock)
                unlessM (hasValidIndex mapper refPath) $
                    createIndex mapper refPath
    return refPath
  where
    hoursToDiffTime h = fromInteger (h * 3600)


lookupReference :: KwArgsValues -> NGLessIO ReferenceInfo
lookupReference args = do
    let reference = lookup "reference" args
        fafile = lookup "fafile" args
    case (reference, fafile) of
        (Nothing, Nothing) -> throwScriptError "Either reference or fafile must be passed"
        (Just _, Just _) -> throwScriptError "Reference and fafile cannot be used simmultaneously"
        (Just r, Nothing) -> PackagedReference <$> stringOrTypeError "reference in map argument" r
        (Nothing, Just fa) -> (FaFile . T.unpack) <$> stringOrTypeError "fafile in map argument" fa


mapToReference :: Mapper -> FilePath -> ReadSet -> [String] -> NGLessIO (FilePath, (Int, Int, Int))
mapToReference mapper refIndex (ReadSet3 _ fp1 fp2 fp3) extraArgs = do
    (out, hout) <- openNGLTempFile refIndex "mapped_concat_" ".sam"
    let out1 = CB.sinkHandle hout
        out2 :: C.Sink B.ByteString IO ()
        out2 = CB.lines
                =$= CL.filter (\line -> not (B.null line) &&  B8.head line /= '@')
                =$= C.unlinesAscii
                =$= CB.sinkHandle hout
    (t0,a0,u0) <- runNGLess =<< callMapper mapper refIndex [fp1, fp2] extraArgs (zipToStats out1)
    (t1,a1,u1) <- runNGLess =<< callMapper mapper refIndex [fp3] extraArgs (zipToStats out2)

    liftIO $ hClose hout
    return (out, (t0+t1,a0+a1, u0+u1))

mapToReference mapper refIndex refs extraArgs = do
    (newfp, hout) <- openNGLTempFile refIndex "mapped_" ".sam"
    outputListLno' DebugOutput ["Write .sam file to: ", newfp]
    let fps = case refs of
            ReadSet1 _ fp -> [fp]
            ReadSet2 _ fp1 fp2 -> [fp1, fp2]
            _ -> error "This case should never happen: mapToReference"
    stats <- runNGLess =<< callMapper mapper refIndex fps extraArgs (zipToStats (C.sinkHandle hout))
    liftIO $ hClose hout
    return (newfp, stats)
zipToStats out = snd <$> C.toConsumer (zipSink2 out (linesC =$= samStatsC))

performMap :: Mapper -> ReferenceInfo -> T.Text -> ReadSet -> [String] -> NGLessIO NGLessObject
performMap mapper ref name rs extraArgs = do
    (ref', defGen') <- indexReference ref
    (samPath', (total, aligned, unique)) <- mapToReference mapper ref' rs extraArgs
    outputMapStatistics (MappingInfo undefined samPath' ref' total aligned unique)
    return $ NGOMappedReadSet name (File samPath') defGen'
    where
        indexReference :: ReferenceInfo -> NGLessIO (FilePath, Maybe T.Text)
        indexReference (FaFile fa) = (,Nothing) <$> ensureIndexExists mapper fa
        indexReference (PackagedReference r) = do
            ReferenceFilePaths fafile _ _ <- ensureDataPresent r
            case fafile of
                Just fp -> (, Just r) <$> ensureIndexExists mapper fp
                Nothing -> throwScriptError ("Could not find reference '" ++ T.unpack r ++ "'.")

_samStats :: FilePath -> NGLessIO (Int, Int, Int)
_samStats fname = samBamConduit fname $$ linesC =$= samStatsC >>= runNGLess

samStatsC :: (MonadIO m) => C.Sink ByteLine m (NGLess (Int, Int, Int))
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
        readSamGroupsC
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
    oAll <- lookupBoolOrScriptErrorDef (return False) "map() call" "mode_all" args
    extraArgs <- map T.unpack <$> lookupStringListOrScriptErrorDef (return []) "extra bwa arguments" "__extra_bwa_args" args
    mapper <- getMapper
    let bwaArgs = extraArgs ++ ["-a" | oAll]
        executeMap' (NGOList es)            = NGOList <$> forM es executeMap'
        executeMap' (NGOReadSet name rs)    = performMap mapper ref name rs bwaArgs
        executeMap' v = throwShouldNotOccur ("map expects ReadSet, got " ++ show v ++ "")
    executeMap' fps

executeMapStats :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeMapStats (NGOMappedReadSet name sami _) _ = do
    outputListLno' TraceOutput ["Computing mapstats on ", show sami]
    let (samfp, stream) = asSamStream sami
    (t, al, u) <- stream $$ samStatsC >>= runNGLess
    (countfp, hout) <- openNGLTempFile samfp "sam_stats_" ".stats"
    liftIO . hPutStr hout . concat $
        [     "\t",  T.unpack name, "\n"
        ,"total\t",   show  t, "\n"
        ,"aligned\t", show al, "\n"
        ,"unique\t",  show  u, "\n"
        ]
    liftIO $ hClose hout
    return $! NGOCounts (File countfp)
executeMapStats other _ = throwScriptError ("Wrong argument for mapstats: "++show other)
