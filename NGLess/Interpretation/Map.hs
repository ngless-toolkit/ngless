{-# LANGUAGE OverloadedStrings #-}


module Interpretation.Map
    ( interpretMapOp
    , configGenome
    , indexReference
    , mapToReference
    , _calcSamStats
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

import System.Directory
import System.FilePath.Posix
import Numeric

import Data.Maybe

import GHC.Conc (numCapabilities)

import System.Process
import System.Exit
import System.IO

import Control.Monad
import Control.Applicative ((<$>))

import SamBamOperations
import Language
import FileManagement
import ReferenceDatabases
import Configuration

import Data.Sam

indexRequiredFormats :: [String]
indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]

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
            case isDefaultGenome r of
                False  -> indexReference r >>= \x -> return (x, Nothing) --user supplies genome
                True   -> do
                    rootGen <- getGenomeDir r
                    findIndexFiles r' >>= \res -> case res of
                        Nothing -> configGenome r' User >>= \x -> return (x, Just rootGen) -- download and install genome on User mode
                        Just p  -> return (p, Just rootGen) -- already installed


{-
    Receives - A default genome name
    Returns  - The root dir for that genome

    If not installed in SU mode, return user mode path since it will be installed on User mode next.
-}
getGenomeDir :: T.Text -> IO T.Text
getGenomeDir n = T.pack <$> do
    dataDir <- globalDataDirectory
    doesExist <- doesDirectoryExist $ dataDir </> getGenomeRootPath n
    case doesExist of
        True  -> return (dataDir </> getGenomeRootPath n)
        False -> (</> getGenomeRootPath n) <$> userDataDirectory


getSamStats :: FilePath -> IO ()
getSamStats fname = readPossiblyCompressedFile fname >>= printSamStats . _calcSamStats

_calcSamStats :: BL.ByteString -> [Int]
_calcSamStats contents = [total', aligned', unique', lowQual']
    where res' = samStats contents
          total' = V.unsafeIndex res' (fromEnum Total)
          aligned' = V.unsafeIndex res' (fromEnum Aligned)
          unique' = V.unsafeIndex res' (fromEnum Unique)
          lowQual' = V.unsafeIndex res' (fromEnum LowQual)

printSamStats stats = do
    putStrLn $ "Total reads: " ++ (show total)
    putStrLn $ "Total reads aligned: " ++ (show aligned) ++ "[" ++ (showFloat' $ calcDiv aligned total) ++ "%]"
    putStrLn $ "Total reads Unique map: " ++ (show unique) ++ "[" ++ (showFloat' $ calcDiv unique aligned) ++ "%]"
    putStrLn $ "Total reads Non-Unique map: " ++ (show $ aligned - unique) ++ "[" ++ (showFloat' $ 100 - (calcDiv unique aligned)) ++ "%]"
    putStrLn $ "Total reads without enough qual: " ++ (show lowQ)
  where
    total   = stats !! 0
    aligned = stats !! 1
    unique  = stats !! 2
    lowQ    = stats !! 3
    showFloat' num = showFFloat (Just numDecimalPlaces) num ""
    calcDiv :: Int -> Int -> Double
    calcDiv a b =
          let x = fromIntegral a
              y = fromIntegral b
          in (x / y) * (100 :: Double)

-- | checks first user and then global data directories for <ref>.
-- The user directory is checked first to allow the user to override a
-- problematic global installation.
findIndexFiles :: FilePath -> IO (Maybe FilePath)
findIndexFiles ref = do
    globalIndex <- findIndexFilesIn ref User
    if isJust globalIndex
        then return globalIndex
        else findIndexFilesIn ref Root

findIndexFilesIn :: FilePath -> InstallMode -> IO (Maybe FilePath)
findIndexFilesIn ref mode = do
    dirPath <- (if mode == Root
                    then globalDataDirectory
                    else userDataDirectory)
    let indexPath = (dirPath </> getIndexPath ref)
    hasIndex <- doAllFilesExist indexPath indexRequiredFormats
    return (if hasIndex
                then Just indexPath
                else Nothing)

configGenome :: FilePath -> InstallMode -> IO FilePath
configGenome ref mode = do
    dir <- (if mode == Root
                then globalDataDirectory
                else userDataDirectory)
    indexPath <- findIndexFilesIn ref mode
    when (isNothing indexPath) $ do
        createDirectoryIfMissing True dir
        installGenome ref dir
    return (dir </> getIndexPath ref)


installGenome :: FilePath -> FilePath -> IO ()
installGenome ref d = do
    downloadReference ref (d </> tarName)
    Tar.unpack d . Tar.read . GZip.decompress =<< LB.readFile ( d </> tarName)
   where
        dirName = case lookup (T.pack ref) defaultGenomes of
            Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
            Just v  -> v
        tarName = dirName <.> "tar.gz"

doAllFilesExist :: String -> [String] -> IO Bool
doAllFilesExist _ [] = return True
doAllFilesExist basepath (x:xs) = do
    isThere <- doesFileExist (basepath ++ x)
    if isThere
        then doAllFilesExist basepath xs
        else return False
