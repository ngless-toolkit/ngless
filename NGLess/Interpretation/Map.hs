{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections #-}

module Interpretation.Map
    ( interpretMapOp
    , interpretMapOp2
    , _calcSamStats
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Monad.Trans.Resource

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

ensureIndexExists :: FilePath -> NGLessIO FilePath
ensureIndexExists refPath = do
    hasIndex <- hasValidIndex refPath
    if hasIndex
        then outputListLno' DebugOutput ["Index for ", refPath, " already exists."]
        else createIndex refPath
    return refPath


mapToReference :: FilePath -> [FilePath] -> NGLessIO String
mapToReference refIndex fps = do
    (rk, (newfp, hout)) <- openNGLTempFile' refIndex "mapped_" ".sam"
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    outputListLno' DebugOutput ["Write .sam file to: ", newfp]
    bwaPath <- bwaBin
    let cmdargs =  ["mem","-t", show numCapabilities, refIndex] ++ fps
    outputListLno' TraceOutput (["Calling binary ", bwaPath, " with args: "] ++ cmdargs)
    (err, exitCode) <- liftIO $ do
        (_, _, Just herr, jHandle) <-
            createProcess (
                proc bwaPath cmdargs
                ) { std_out = UseHandle hout,
                    std_err = CreatePipe }
        err <- hGetContents herr
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

interpretMapOp :: T.Text -> FilePath -> NGLessIO NGLessObject
interpretMapOp r ds = interpretMapOp' r [ds]
interpretMapOp2 :: T.Text -> FilePath -> FilePath -> NGLessIO NGLessObject
interpretMapOp2 r mate1 mate2 = interpretMapOp' r [mate1, mate2]

interpretMapOp' :: T.Text -> [FilePath] -> NGLessIO NGLessObject
interpretMapOp' r ds = do
    (ref', defGen') <- indexReference' (T.unpack r)
    samPath' <- mapToReference ref' ds
    getSamStats samPath'
    return $ NGOMappedReadSet samPath' defGen'
    where
        indexReference' :: FilePath -> NGLessIO (FilePath, Maybe T.Text)
        indexReference' r'
            | isDefaultReference r' = do
                    basedir  <- ensureDataPresent r'
                    return (getIndexPath basedir, Just r)
            | otherwise  = (, Nothing) <$> ensureIndexExists r'


getSamStats :: FilePath -> NGLessIO ()
getSamStats fname = liftIO (readPossiblyCompressedFile fname) >>= printSamStats . _calcSamStats

data P4 = P4 !Integer !Integer !Integer !Integer

_calcSamStats :: BL.ByteString -> (Integer,Integer,Integer,Integer)
_calcSamStats contents = (total, aligned, unique, lowQual)
    where
        P4 total aligned unique lowQual = computeStats . readAlignments $ contents
        computeStats = foldl update (P4 0 0 0 0)
        update (P4 t al u lQ) samLine =
            P4 (t + 1)
                (al + (asInteger . isAligned $ samLine))
                (u  + (asInteger . isUnique $ samLine))
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

