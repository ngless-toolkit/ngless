{-# LANGUAGE OverloadedStrings #-}


module MapInterpretOperation
    (
    interpretMapOp
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Data.Vector.Unboxed as V

import System.Directory
import System.FilePath.Posix

import Numeric
import InvokeExternalProgs
import SamBamOperations
import Language
import FileManagement


-- Constants 

defGenomeDir :: FilePath
defGenomeDir = "../share/ngless/genomes"

defaultGenomes :: [T.Text]
defaultGenomes = ["hg19"]

numDecimalPlaces :: Int
numDecimalPlaces = 2

----

isDefaultGenome :: T.Text -> Bool
isDefaultGenome name = name `elem` defaultGenomes

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
        True -> indexReference $ T.pack (genomePath </> ref </> ".fa")
        False -> downloadReference (ref </> ".fa") genomePath
    -- run under script environment
    setCurrentDirectory scriptEnvDir'


downloadReference fname path = do
    return ()

