{-# LANGUAGE OverloadedStrings #-}


module MapInterpretOperation
    (
    interpretMapOp
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Data.Vector.Unboxed as V
import qualified Data.Map as Map

import System.Directory
import System.FilePath.Posix

import Data.Maybe
import Data.Conduit
import Data.Conduit.Binary (sinkFile)

import Network.HTTP.Conduit

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

defaultGenomes :: [(T.Text, FilePath)]
defaultGenomes = [("hg19", "http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz")]

numDecimalPlaces :: Int
numDecimalPlaces = 2

----

isDefaultGenome :: T.Text -> Bool
isDefaultGenome name = name `elem` (map fst defaultGenomes)

interpretMapOp ref ds = do
    case isDefaultGenome ref of
        False  -> indexReference ref
        True   -> configGenome (T.unpack ref)
    execMap' <- mapToReference ref (B.unpack ds)
    getSamStats execMap'
    return execMap'


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


configGenome :: FilePath -> IO ()
configGenome ref = do
    scriptEnvDir' <- getCurrentDirectory
    switchToNglessRoot
    -- run under ngless root environment
    let genomePath = defGenomeDir </> ref
    _ <- createDirectoryIfMissing True genomePath
    res <- doesDirContainFormats genomePath  [".fa"] -- should check for fasta or fa
    case res of 
        True -> indexReference $ T.pack (genomePath </> ref ++ ".fa")
        False -> do
            let url = fromJust $ Map.lookup (T.pack ref) (Map.fromList defaultGenomes)
            downloadReference url (genomePath </> ref ++ ".fa")
    -- run under script environment
    setCurrentDirectory scriptEnvDir'


downloadReference url destPath = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    req <- liftIO $ parseUrl url
    res <- http req manager
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

