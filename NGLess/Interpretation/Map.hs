{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections #-}

module Interpretation.Map
    ( executeMap
    , _samStats
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Control.Monad
import           Control.Monad.Trans.Resource

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as C
import           Data.Conduit (($$), (=$=))

import GHC.Conc     (getNumCapabilities)
import Data.List    (find)
import Numeric      (showFFloat)
import Data.Bits    (testBit)

import System.Process
import System.IO
import Data.Function
import System.Exit

import Control.Monad.IO.Class (liftIO)

import Language
import FileManagement
import ReferenceDatabases
import Configuration
import Modules
import Output
import NGLess

import Utils.Bwa
import Utils.LockFile
import Utils.Utils (readProcessErrorWithExitCode)
import Utils.Conduit (conduitPossiblyCompressedFile)

data ReferenceInfo = PackagedReference T.Text | FaFile FilePath

lookupReference :: KwArgsValues -> NGLessIO ReferenceInfo
lookupReference args = do
    let reference = lookup "reference" args
        fafile = lookup "fafile" args
    case (reference, fafile) of
        (Nothing, Nothing) -> throwScriptError ("Either reference or fafile must be passed" :: String)
        (Just _, Just _) -> throwScriptError ("Reference and fafile cannot be used simmultaneously" :: String)
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

mapToReference :: FilePath -> [FilePath] -> [String] -> NGLessIO FilePath
mapToReference refIndex [fp1,fp2, fp3] extraArgs = do
    fp0 <- mapToReference refIndex [fp1, fp2] extraArgs
    fp1 <- mapToReference refIndex [fp3] extraArgs
    mergeSams refIndex fp0 fp1

mapToReference refIndex fps extraArgs = do
    (rk, (newfp, hout)) <- openNGLTempFile' refIndex "mapped_" ".sam"
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    outputListLno' DebugOutput ["Write .sam file to: ", newfp]
    bwaPath <- bwaBin
    numCapabilities <- liftIO getNumCapabilities
    let cmdargs =  concat [["mem", "-t", show numCapabilities, refIndex], extraArgs, fps]
    outputListLno' TraceOutput ["Calling binary ", bwaPath, " with args: ", unwords cmdargs]
    let cp = (proc bwaPath cmdargs) { std_out = UseHandle hout }
    (err, exitCode) <- liftIO $ readProcessErrorWithExitCode cp
    liftIO $ hClose hout
    outputListLno' DebugOutput ["BWA info: ", err]
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done mapping to ", refIndex]
            return newfp
        ExitFailure code -> do
            release rk
            throwSystemError $ concat (["Failed mapping\nCommand line was::\n\t",
                            bwaPath, " mem -t ", show numCapabilities, " '", refIndex, "' '"] ++ fps ++ ["'\n",
                            "Bwa error code was ", show code, "."])

interpretMapOp :: ReferenceInfo -> T.Text -> [FilePath] -> [String] -> NGLessIO NGLessObject
interpretMapOp ref name fps extraArgs = do
    (ref', defGen') <- indexReference ref
    samPath' <- mapToReference ref' fps extraArgs
    printMappingStats samPath'
    return $ NGOMappedReadSet name samPath' defGen'
    where
        indexReference :: ReferenceInfo -> NGLessIO (FilePath, Maybe T.Text)
        indexReference (FaFile fa) = (,Nothing) <$> ensureIndexExists fa
        indexReference (PackagedReference r) = case getBuiltinReference r of
            Just r' -> do
                basedir  <- ensureDataPresent (T.unpack r)
                return (buildGenomePath basedir, Just r)
            Nothing -> do
                externalRef <- ensureIndexExists =<< findExternalReference r
                return (externalRef, Just r)

findExternalReference :: T.Text -> NGLessIO FilePath
findExternalReference rname = do
    mods <- loadedModules
    let refs = concatMap modReferences mods
    case find ((==rname) . erefName) refs of
        Just rinfo -> return . faFile $ rinfo
        Nothing -> throwScriptError $ T.concat ["Could not find reference '", rname, "'. It is not builtin nor in one of the loaded modules."]

_samStats :: FilePath -> NGLessIO (Int, Int, Int)
_samStats fname = do
    let add1if !v True = v+1
        add1if !v False = v
        update _ [] = error "This is a bug in ngless" -- perhaps readSamGroupsC should use NonEmptyList
        update (!t,!al,!u) ((_,aligned):rest) =
            (t + 1
                ,add1if al aligned
                ,add1if u  (aligned && null rest))
        -- This is ugly code, but it makes a big difference in performance
        -- partialSamParse :: (MonadError NGError m) => C.Conduit B.ByteString m (B.ByteString, B.ByteString)
        partialSamParse = C.awaitForever $ \line ->
            unless (B8.head line == '@') $
                case B8.elemIndex '\t' line of
                    Nothing -> throwDataError ("Cannot parse SAM file: '"++fname++"'")
                    Just tab1 -> let (seqname,rest) = B.splitAt (tab1+1) line in -- splitting at tab1 will remove the tab
                        case B8.readInt rest of
                            Nothing -> throwDataError ("Cannot parse flags in SAM file '"++B8.unpack rest ++"'")
                            Just (v,_) -> C.yield (seqname, not $ v `testBit` 2)

    conduitPossiblyCompressedFile fname
        =$= CB.lines
        =$= partialSamParse
        =$= CL.groupBy ((==) `on` fst)
        $$ CL.fold update (0,0,0)

printMappingStats :: FilePath -> NGLessIO ()
printMappingStats fname = do
    (total,aligned,unique) <- _samStats fname
    let out = outputListLno' ResultOutput
    out ["Total reads: ", show total]
    out ["Total reads aligned: ", showNumAndPercentage aligned total]
    out ["Total reads Unique map: ", showNumAndPercentage unique total]
    out ["Total reads Non-Unique map: ", showNumAndPercentage (aligned - unique) total]

showNumAndPercentage :: Int  -> Int  -> String
showNumAndPercentage v 0 = showNumAndPercentage v 1 -- same output & avoid division by zero
showNumAndPercentage v total =
    concat [show v, " [", showFFloat (Just 2) ((fromIntegral (100*v) / fromIntegral total) :: Double) "", "%]"]

executeMap :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeMap fps args = do
    ref <- lookupReference args
    extraArgs <- (map T.unpack) <$> lookupStringListOrScriptErrorDef (return []) "extra bwa arguments" "__extra_bwa_args" args
    let executeMap' (NGOList es) = NGOList <$> forM es executeMap'
        executeMap' (NGOReadSet name (ReadSet1 _enc file))   = interpretMapOp ref name [file] extraArgs
        executeMap' (NGOReadSet name (ReadSet2 _enc fp1 fp2)) = interpretMapOp ref name [fp1,fp2] extraArgs
        executeMap' (NGOReadSet name (ReadSet3 _enc fp1 fp2 fp3)) = interpretMapOp ref name [fp1,fp2,fp3] extraArgs
        executeMap' v = throwShouldNotOccur ("map expects ReadSet, got " ++ show v ++ "")
    executeMap' fps
