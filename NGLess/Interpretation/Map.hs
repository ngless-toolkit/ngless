{-# LANGUAGE TupleSections #-}


module Interpretation.Map
    ( interpretMapOp
    , indexReference
    , mapToReference
    , _calcSamStats
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

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

import Data.Sam
import Utils.Bwa

indexReference :: T.Text -> IO FilePath
indexReference refPath = do
    let refPath' = (T.unpack refPath)
    hasIndex <- hasValidIndex refPath'
    if hasIndex
        then printNglessLn $ "index for " ++ refPath' ++ " already exists."
        else createIndex refPath'
    return refPath'


mapToReference :: T.Text -> FilePath -> IO String
mapToReference refIndex readSet = do
    bwaPath <- bwaBin
    newfp <- getTempFilePath readSet
    let newfp' = newfp ++ ".sam"
    printNglessLn $ "write .sam file to: " ++ (show newfp')
    withFile newfp' WriteMode $ \hout -> do
        (_, _, Just herr, jHandle) <-
            createProcess (
                proc bwaPath
                    ["mem","-t",(show numCapabilities),(T.unpack refIndex), readSet]
                ) { std_out = UseHandle hout,
                    std_err = CreatePipe }
        err <- hGetContents herr
        putStrLn $ concat ["Error in bwa: ", err]
        exitCode <- waitForProcess jHandle
        hClose herr
        case exitCode of
           ExitSuccess -> return newfp'
           ExitFailure code -> error ("Failure on mapping against reference:" ++ show code)


numDecimalPlaces :: Int
numDecimalPlaces = 2


interpretMapOp :: T.Text -> FilePath -> IO NGLessObject
interpretMapOp r ds = do
    (ref', defGen') <- indexReference'
    samPath' <- mapToReference (T.pack ref') ds
    getSamStats samPath'
    return $ NGOMappedReadSet samPath' defGen'
    where
        r' = T.unpack r
        indexReference' :: IO (FilePath, Maybe T.Text)
        indexReference' =
            if isDefaultReference (T.unpack r)
                then do
                    gen <- ensureDataPresent r'
                    return (gen, Just r)
                else (, Nothing) <$> indexReference r

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
    putStrLn $ "Total reads: " ++ (show total)
    putStrLn $ "Total reads aligned: " ++ (show aligned) ++ "[" ++ (showFloat' $ calcDiv aligned total) ++ "%]"
    putStrLn $ "Total reads Unique map: " ++ (show unique) ++ "[" ++ (showFloat' $ calcDiv unique aligned) ++ "%]"
    putStrLn $ "Total reads Non-Unique map: " ++ (show $ aligned - unique) ++ "[" ++ (showFloat' $ 100 - (calcDiv unique aligned)) ++ "%]"
    putStrLn $ "Total reads without enough qual: " ++ (show lowQ)
  where
    showFloat' num = showFFloat (Just numDecimalPlaces) num ""
    calcDiv :: Integer -> Integer -> Double
    calcDiv a b =
          let x = fromIntegral a
              y = fromIntegral b
          in (x / y) * (100 :: Double)

