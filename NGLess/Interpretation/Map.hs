{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections #-}

module Interpretation.Map
    ( executeMap
    , _calcSamStats
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Concurrent
import Control.Exception (evaluate)
import Data.Maybe
import Numeric

import GHC.Conc (numCapabilities)

import System.Process
import System.Exit
import System.IO

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import Language
import FileManagement
import ReferenceDatabases
import Configuration
import Output
import NGLess

import Data.Sam
import Utils.Bwa
import Utils.Utils (readPossiblyCompressedFile)

data Reference = Reference FilePath | FaFile FilePath

lookupReference :: KwArgsValues -> NGLessIO Reference
lookupReference args = do
    let reference = lookup "reference" args
        fafile = lookup "fafile" args
    case (reference, fafile) of
        (Nothing, Nothing) -> throwScriptError ("Either reference or fafile must be passed" :: String)
        (Just _, Just _) -> throwScriptError ("Reference and fafile cannot be used simmultaneously" :: String)
        (Just r, Nothing) -> (Reference . T.unpack) <$> stringOrTypeError "reference in map argument" r
        (Nothing, Just fa) -> (FaFile . T.unpack) <$> stringOrTypeError "fafile in map argument" fa

ensureIndexExists :: FilePath -> NGLessIO FilePath
ensureIndexExists refPath = do
    hasIndex <- hasValidIndex refPath
    if hasIndex
        then outputListLno' DebugOutput ["Index for ", refPath, " already exists."]
        else createIndex refPath
    return refPath


mapToReference :: FilePath -> [FilePath] -> [String] -> NGLessIO String
mapToReference refIndex fps extraArgs = do
    (rk, (newfp, hout)) <- openNGLTempFile' refIndex "mapped_" ".sam"
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    outputListLno' DebugOutput ["Write .sam file to: ", newfp]
    bwaPath <- bwaBin
    let cmdargs =  concat [["mem", "-t", show numCapabilities, refIndex], extraArgs, fps]
    outputListLno' TraceOutput ["Calling binary ", bwaPath, " with args: ", unwords cmdargs]
    (err, exitCode) <- liftIO $ do
        (_, _, Just herr, jHandle) <-
            createProcess (
                proc bwaPath cmdargs
                ) { std_out = UseHandle hout,
                    std_err = CreatePipe }
        err <- hGetContents herr
        -- In a separate thread, consume all the error input
        -- the same pattern is used in the implementation of
        -- readProcessWithErrorCode (which cannot be used here as we want to
        -- use `hout` for stdout)
        void . forkIO $ evaluate (length err) >> return ()
        exitCode <- waitForProcess jHandle
        hClose herr
        return (err, exitCode)
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

interpretMapOp :: Reference -> [FilePath] -> [String] -> NGLessIO NGLessObject
interpretMapOp ref ds extraArgs = do
    (ref', defGen') <- indexReference ref
    samPath' <- mapToReference ref' ds extraArgs
    getSamStats samPath'
    return $ NGOMappedReadSet samPath' defGen'
    where
        indexReference :: Reference -> NGLessIO (FilePath, Maybe T.Text)
        indexReference (FaFile fa) = (,Nothing) <$> ensureIndexExists fa
        indexReference (Reference r) = do
                basedir  <- ensureDataPresent r
                return (buildGenomePath basedir, Just $ T.pack r)


getSamStats :: FilePath -> NGLessIO ()
getSamStats fname = liftIO (readPossiblyCompressedFile fname) >>= printSamStats . _calcSamStats

data P4 = P4 !Integer !Integer !Integer !Integer

_calcSamStats :: BL.ByteString -> (Integer,Integer,Integer,Integer)
_calcSamStats contents = (total, aligned, unique, lowQual)
    where
        P4 total aligned unique lowQual = computeStats . readAlignments $ contents
        readAlignments :: BL.ByteString -> [SamLine]
        readAlignments = mapMaybe readSamLine' . BL8.lines
        readSamLine' line = case readSamLine line of
            Left err -> error (show err)
            Right SamHeader{} -> Nothing
            Right v@SamLine{} -> Just v
        computeStats = foldl update (P4 0 0 0 0)
        update (P4 t al u lQ) samLine =
            P4 (t + 1)
                (al + (asInteger . isAligned $ samLine))
                0 --(u  + (asInteger . isUnique $ samLine))
                (lQ + (asInteger . hasQual $ samLine))
        asInteger True = 1
        asInteger False = 0

printSamStats (total, aligned, unique, lowQ) = do
    out ["Total reads: ", show total]
    out ["Total reads aligned: ", show aligned, "[", showFloat' $ calcDiv aligned total, "%]"]
    out ["Total reads Unique map: ", show unique, "[", showFloat' $ calcDiv unique aligned, "%]"]
    out ["Total reads Non-Unique map: ", show $ aligned - unique, "[", showFloat' $ 100 - (calcDiv unique aligned), "%]"]
    out ["Total reads without enough qual: ", show lowQ]
  where
    out = outputListLno' ResultOutput
    showFloat' num = showFFloat (Just 2) num ""
    calcDiv :: Integer -> Integer -> Double
    calcDiv a b =
          let x = fromIntegral a
              y = fromIntegral b
          in (x / y) * (100 :: Double)

executeMap :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeMap fps args = do
    ref <- lookupReference args
    extraArgs <- (map T.unpack) <$> lookupStringListOrScriptErrorDef (return []) "extra bwa arguments" "__extra_bwa_args" args
    let executeMap' (NGOList es) = NGOList <$> forM es executeMap'
        executeMap' (NGOReadSet1 _enc file)    = interpretMapOp ref [file] extraArgs
        executeMap' (NGOReadSet2 _enc fp1 fp2) = interpretMapOp ref [fp1,fp2] extraArgs
        executeMap' v = throwShouldNotOccur ("map of " ++ show v ++ " not implemented yet")
    executeMap' fps
