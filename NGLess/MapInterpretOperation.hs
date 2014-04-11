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

import Data.Maybe
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



-- Constants 

defGenomeDir :: FilePath
defGenomeDir = "../share/ngless/genomes"

genomesRep :: FilePath
genomesRep = "UCSC"

ucscUrl :: FilePath
ucscUrl = "http://kdbio.inesc-id.pt/~prrm/genomes"

getUcscUrl :: FilePath -> FilePath
getUcscUrl genome = do
    let genomeMap = Map.fromList defaultGenomes
        i = Map.lookupIndex (T.pack genome) genomeMap
    case i of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just index -> do
            let res =  Map.elemAt index genomeMap 
            ucscUrl </>  (getGenomeDirName res) ++ ".tar.gz"

getGenomeDirName :: (T.Text, FilePath) -> FilePath
getGenomeDirName (a,d) = d ++ "_" ++ genomesRep ++ "_" ++ (T.unpack a)

getGenomeRootPath :: (T.Text, FilePath) -> FilePath
getGenomeRootPath (a,d) = d </> genomesRep </> (T.unpack a)

getGenomePath :: FilePath -> FilePath
getGenomePath gen = do
    case Map.lookupIndex (T.pack gen) mapGens of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just index -> do
            let res =  Map.elemAt index mapGens 
            getGenomeDirName res </> getGenomeRootPath res </> "Sequence/BWAIndex/genome.fa"
    where
        mapGens = Map.fromList defaultGenomes

defaultGenomes :: [(T.Text, FilePath)]
defaultGenomes = [
                    ("hg19", "Homo_sapiens"),
                    ("mm10", "Mus_musculus"),
                    ("rn4",  "Rattus_norvegicus"),
                    ("bosTau4",  "Bos_taurus"),   
                    ("canFam2","Canis_familiaris"),
                    ("dm3","Drosophila_melanogaster"),
                    ("TAIR10","Arabidopsis_thaliana"),
                    ("ce10","Caenorhabditis_elegans"),
                    ("sacCer3","Saccharomyces_cerevisiae")
                 ]


numDecimalPlaces :: Int
numDecimalPlaces = 2

----

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
    printNglessLn $ "Total reads: " ++ (show total')
    printNglessLn $ "Total reads aligned: " ++ (show aligned') ++ "[" ++ (showFloat' $ calcDiv aligned' total') ++ "%]"
    printNglessLn $ "Total reads Unique map: " ++ (show unique') ++ "[" ++ (showFloat' $ calcDiv unique' aligned') ++ "%]"
    printNglessLn $ "Total reads Non-Unique map: " ++ (show $ aligned' - unique') ++ "[" ++ (showFloat' $ 100 - (calcDiv unique' aligned')) ++ "%]"


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
    
    let genomePath = defGenomeDir </> getGenomePath ref
    res <- doesFileExist genomePath -- should check for fasta or fa
    when (not res) $ do 
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
        tarName = dirName ++ ".tar.gz"


downloadReference url destPath = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    req <- liftIO $ parseUrl url
    res <- http req manager
    liftIO (putStrLn $ show (responseHeaders res))
    let genSize = B.unpack $ fromJust $ Map.lookup "Content-Length" (Map.fromList $ responseHeaders res)
    responseBody res $$+- printProgress (read genSize :: Int) =$ sinkFile destPath
    liftIO $ putProgress "Genome download completed!"

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

