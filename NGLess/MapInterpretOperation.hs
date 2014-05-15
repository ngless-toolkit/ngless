{-# LANGUAGE OverloadedStrings #-}


module MapInterpretOperation
    (
    interpretMapOp
    , configGenome
    , calcSamStats
    ) where

import qualified UnpackIlluminaGenomes as MTar

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map

import System.Directory
import System.FilePath.Posix

import Data.Conduit
import Data.Maybe
import Data.Conduit.Binary (sinkFile)

import Network.HTTP.Conduit

import Control.Monad
import Control.Monad.Error (liftIO)

import ProgressBar
import Numeric
import InvokeExternalProgs
import SamBamOperations
import Language
import FileManagement
import UnpackIlluminaGenomes

import Data.DefaultValues
import Data.Sam


numDecimalPlaces :: Int
numDecimalPlaces = 2

isDefaultGenome :: T.Text -> Bool
isDefaultGenome name = name `elem` (map fst defaultGenomes)

interpretMapOp ref ds = do
    ref' <- indexReference' ref
    execMap' <- mapToReference (T.pack ref') (B.unpack ds)
    getSamStats execMap'
    return execMap'
    where 
        indexReference' r = case isDefaultGenome r of
                                False  -> indexReference r
                                True   -> do
                                    res <- isIndexCalculated (T.unpack r) 
                                    case res of 
                                        Nothing -> configGenome (T.unpack r) User
                                        Just p  -> return p

getSamStats (NGOMappedReadSet fname) = unCompress (T.unpack fname) >>= printSamStats . calcSamStats
getSamStats err = error $ "Type must be NGOMappedReadSet, but is: " ++ (show err)


calcSamStats contents = do
    let res' = samStats contents
        total' = getV res' (fromEnum Total)
        aligned' = getV res' (fromEnum Aligned)
        unique' = getV res' (fromEnum Unique)
    [total', aligned', unique']

printSamStats stats = do
    putStrLn $ "Total reads: " ++ (show total)
    putStrLn $ "Total reads aligned: " ++ (show aligned) ++ "[" ++ (showFloat' $ calcDiv aligned total) ++ "%]"
    putStrLn $ "Total reads Unique map: " ++ (show unique) ++ "[" ++ (showFloat' $ calcDiv unique aligned) ++ "%]"
    putStrLn $ "Total reads Non-Unique map: " ++ (show $ aligned - unique) ++ "[" ++ (showFloat' $ 100 - (calcDiv unique aligned)) ++ "%]"
  where
    total = stats !! 0
    aligned = stats !! 1
    unique = stats !! 2

getV vec i =  V.unsafeIndex vec i

calcDiv :: Int -> Int -> Double
calcDiv a b = 
      let x = fromIntegral a
          y = fromIntegral b
      in (x / y) * (100 :: Double) 

showFloat' num = showFFloat (Just numDecimalPlaces) num ""

-- check both SU and normal user Genomes dir for <ref> 
isIndexCalculated :: FilePath -> IO (Maybe FilePath)
isIndexCalculated ref = do
    -- super user genomes dir --
    hasSUFiles <- isIndexCalcAux ref Root
    -- normal user genomes dir --
    hasFiles <- isIndexCalcAux ref User

    case isJust hasSUFiles of
        True  -> return hasSUFiles -- installed in SU dir
        False -> case isJust hasFiles of
                True  -> return hasFiles -- installed in $HOME
                False -> return Nothing -- not installed


isIndexCalcAux :: FilePath -> InstallMode -> IO (Maybe FilePath)
isIndexCalcAux ref Root = do
    nglessRoot' <- getNglessRoot
    let dirPath = nglessRoot' </> suGenomeDir
    hasIndex <- doesDirContainFormats (dirPath </> getIndexPath ref) indexRequiredFormats
    case hasIndex of
        True  -> return $ (Just $ dirPath </> getIndexPath ref)
        False -> return Nothing

isIndexCalcAux ref User = do
    defGenomeDir' <- defGenomeDir
    hasIndex <- doesDirContainFormats (defGenomeDir' </> getIndexPath ref) indexRequiredFormats
    case hasIndex of
        True  -> return $ (Just $ defGenomeDir' </> getIndexPath ref)
        False -> return Nothing

configGenome :: FilePath -> InstallMode -> IO FilePath
configGenome ref Root = do
    nglessRoot' <- getNglessRoot
    let dirPath = nglessRoot' </> suGenomeDir
    installGenome' dirPath ref Root

configGenome ref User = do
    defGenomeDir' <- defGenomeDir
    installGenome' defGenomeDir' ref User

installGenome' p ref mode = do
    hasIndex <- isIndexCalcAux ref mode

    when (isNothing hasIndex) $ do 
        createDirectoryIfMissing True p
        installGenome ref p
    
    return (p </> getIndexPath ref)


installGenome ref d = do
    let url = getUcscUrl ref
    downloadReference url (d </> tarName)
    MTar.unpack (d </> dirName) . Tar.read . GZip.decompress =<< LB.readFile (d </> tarName)
   where 
        mapGens = Map.fromList defaultGenomes
        dirName = case Map.lookupIndex (T.pack ref) mapGens of
            Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
            Just index -> do
                let res =  Map.elemAt index mapGens
                getGenomeDirName res
        tarName = dirName <.> "tar.gz"

downloadReference url destPath = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    req <- liftIO $ parseUrl url
    res <- http req manager
    case Map.lookup "Content-Length" (Map.fromList $ responseHeaders res) of
        Nothing -> error ("Error on http request")
        Just genSize -> do
            responseBody res $$+- printProgress (read (B.unpack genSize) :: Int) =$ sinkFile destPath
            liftIO $ putStrLn " Genome download completed! "

printProgress :: Int -> Conduit B.ByteString (ResourceT IO) B.ByteString
printProgress genSize = loop 0
  where
    loop len = await >>= maybe (return ()) 
        (\bs -> do
            let len' = len + B.length bs
                progress = fromIntegral len' / fromIntegral genSize
            liftIO (putProgress $ drawProgressBar 40 progress ++ " " ++ drawPercentage progress)
            yield bs
            loop len'
        )

