{-# LANGUAGE TupleSections #-}


module Interpretation.Map
    ( interpretMapOp
    , indexReference
    , mapToReference
    , _calcSamStats
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

import Numeric

import GHC.Conc (numCapabilities)

import System.Process
import System.Exit
import System.IO

import Control.Applicative ((<$>))

import SamBamOperations
import Language
import FileManagement
import ReferenceDatabases
import Configuration

import Data.Sam

indexReference :: T.Text -> IO FilePath
indexReference refPath = do
    let refPath' = (T.unpack refPath)
    res <- doAllFilesExist refPath' indexRequiredFormats
    case res of
        False -> do
            bwaPath <- bwaBin
            (exitCode, out, err) <-
                readProcessWithExitCode bwaPath ["index", refPath'] []
            printNglessLn err
            printNglessLn out
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _err -> error err
        True -> printNglessLn $ "index for " ++ refPath' ++ " already exists."
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
           ExitFailure code -> error ("Failure on mapping against reference:" ++ (show code))


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
            if isDefaultReference r
                then do
                    gen <- ensureDataPresent r'
                    return (gen, Just r)
                else (, Nothing) <$> indexReference r

getSamStats :: FilePath -> IO ()
getSamStats fname = readPossiblyCompressedFile fname >>= printSamStats . _calcSamStats

_calcSamStats :: BL.ByteString -> (Int,Int,Int,Int)
_calcSamStats contents = (total', aligned', unique', lowQual')
    where res' = samStats contents
          total' = V.unsafeIndex res' (fromEnum Total)
          aligned' = V.unsafeIndex res' (fromEnum Aligned)
          unique' = V.unsafeIndex res' (fromEnum Unique)
          lowQual' = V.unsafeIndex res' (fromEnum LowQual)

printSamStats (total, aligned, unique, lowQ) = do
    putStrLn $ "Total reads: " ++ (show total)
    putStrLn $ "Total reads aligned: " ++ (show aligned) ++ "[" ++ (showFloat' $ calcDiv aligned total) ++ "%]"
    putStrLn $ "Total reads Unique map: " ++ (show unique) ++ "[" ++ (showFloat' $ calcDiv unique aligned) ++ "%]"
    putStrLn $ "Total reads Non-Unique map: " ++ (show $ aligned - unique) ++ "[" ++ (showFloat' $ 100 - (calcDiv unique aligned)) ++ "%]"
    putStrLn $ "Total reads without enough qual: " ++ (show lowQ)
  where
    showFloat' num = showFFloat (Just numDecimalPlaces) num ""
    calcDiv :: Int -> Int -> Double
    calcDiv a b =
          let x = fromIntegral a
              y = fromIntegral b
          in (x / y) * (100 :: Double)

