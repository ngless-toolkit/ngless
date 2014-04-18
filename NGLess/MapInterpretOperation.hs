{-# LANGUAGE OverloadedStrings #-}


module MapInterpretOperation
    (
    interpretMapOp
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
                                True   -> configGenome (T.unpack r)


getSamStats (NGOMappedReadSet fname) = do
    contents <- unCompress (T.unpack fname)
    let res' = samStats contents
        total' = getV res' (fromEnum Total)
        aligned' = getV res' (fromEnum Aligned)
        unique' = getV res' (fromEnum Unique)
    putStrLn $ "Total reads: " ++ (show total')
    putStrLn $ "Total reads aligned: " ++ (show aligned') ++ "[" ++ (showFloat' $ calcDiv aligned' total') ++ "%]"
    putStrLn $ "Total reads Unique map: " ++ (show unique') ++ "[" ++ (showFloat' $ calcDiv unique' aligned') ++ "%]"
    putStrLn $ "Total reads Non-Unique map: " ++ (show $ aligned' - unique') ++ "[" ++ (showFloat' $ 100 - (calcDiv unique' aligned')) ++ "%]"


getSamStats err = error $ "Type must be NGOMappedReadSet, but is: " ++ (show err)

getV vec i =  V.unsafeIndex vec i

calcDiv :: Int -> Int -> Double
calcDiv a b = 
      let x = fromIntegral a
          y = fromIntegral b
      in (x / y) * (100 :: Double) 

showFloat' num = showFFloat (Just numDecimalPlaces) num ""


configGenome :: FilePath -> IO FilePath
configGenome ref = do
    scriptEnvDir' <- getCurrentDirectory
    switchToNglessRoot

    createDirectoryIfMissing True defGenomeDir
    nglessPath <- getCurrentDirectory 
    let genomePath = nglessPath </> defGenomeDir </> getIndexPath ref

    hasFiles <- doesDirContainFormats genomePath indexRequiredFormats

    when (not hasFiles) $ do 
        let url = getUcscUrl ref
        downloadReference url (defGenomeDir </> tarName)
        switchToDir defGenomeDir
        MTar.unpack dirName . Tar.read . GZip.decompress =<< LB.readFile tarName

    setCurrentDirectory scriptEnvDir'
    return genomePath
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

