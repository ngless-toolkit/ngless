{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections #-}

module Interpretation.Map
    ( interpretMapOp
    , _calcSamStats
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Monad.Trans.Resource
import Control.Monad.Trans (liftIO)

import Numeric

import GHC.Conc (numCapabilities)

import System.Process
import System.Exit
import System.IO

import Control.Applicative ((<$>))

import Language
import FileManagement
import ReferenceDatabases
import Configuration
import Output

import Data.Sam
import Utils.Bwa
import Utils.Tempfile

ensureIndexExists :: FilePath -> IO FilePath
ensureIndexExists refPath = do
    hasIndex <- hasValidIndex refPath
    if hasIndex
        then outputListLno' DebugOutput ["Index for ", refPath, " already exists."]
        else createIndex refPath
    return refPath


mapToReference :: FilePath -> FilePath -> ResourceT IO String
mapToReference refIndex readSet = do
    (rk, (newfp, hout)) <- tempfile "mappedOutput.sam"
    liftIO $ do
        outputListLno' InfoOutput ["Starting mapping to ", refIndex]
        bwaPath <- bwaBin
        outputListLno' DebugOutput ["Write .sam file to: ", newfp]
        (_, _, Just herr, jHandle) <-
            createProcess (
                proc bwaPath
                    ["mem","-t",(show numCapabilities), refIndex, readSet]
                ) { std_out = UseHandle hout,
                    std_err = CreatePipe }
        err <- hGetContents herr
        outputListLno' DebugOutput $ ["BWA info: ", err]
        exitCode <- waitForProcess jHandle
        hClose herr
        case exitCode of
            ExitSuccess -> do
                outputListLno' InfoOutput ["Done mapping to ", refIndex]
                return newfp
            ExitFailure code -> do
                release rk
                error $ concat ["Failed mapping\nCommand line was::\n\t",
                                bwaPath, " mem -t ", show numCapabilities, " '", refIndex, "' '", readSet, "'\n",
                                "Bwa error code was ", show code, "."]

interpretMapOp :: T.Text -> FilePath -> ResourceT IO NGLessObject
interpretMapOp r ds = do
    (ref', defGen') <- indexReference'
    samPath' <- mapToReference ref' ds
    liftIO $ getSamStats samPath'
    return $ NGOMappedReadSet samPath' defGen'
    where
        r' = T.unpack r
        indexReference' :: ResourceT IO (FilePath, Maybe T.Text)
        indexReference' = liftIO $
            if isDefaultReference (T.unpack r)
                then do
                    basedir  <- ensureDataPresent r'
                    return (getIndexPath basedir, Just r)
                else (, Nothing) <$> ensureIndexExists r'

getSamStats :: FilePath -> IO ()
getSamStats fname = readPossiblyCompressedFile fname >>= printSamStats . _calcSamStats

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

