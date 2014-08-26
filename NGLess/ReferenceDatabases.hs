{-# LANGUAGE OverloadedStrings #-}

module ReferenceDatabases
    ( isDefaultGenome
    , defaultGenomes
    , downloadURL
    , getGenomeRootPath
    , getIndexPath
    , getGff
    ) where

import qualified Data.Text as T
import System.FilePath( (</>), (<.>) )
import Configuration

bwaIndexPath :: FilePath
bwaIndexPath = "Sequence/BWAIndex"

gffPath :: FilePath
gffPath = "Annotation/annot.gtf.gz"

getGff :: T.Text -> FilePath
getGff n = (T.unpack n) </> gffPath

defaultGenomes :: [(T.Text, FilePath)]
defaultGenomes = [
                    ("hg19", "Homo_sapiens"),
                    ("mm10", "Mus_musculus"),
                    ("rn4",  "Rattus_norvegicus"),
                    ("bosTau4",  "Bos_taurus"),   
                    ("canFam2","Canis_familiaris"),
                    ("dm3","Drosophila_melanogaster"),
                    ("ce10","Caenorhabditis_elegans"),
                    ("sacCer3","Saccharomyces_cerevisiae")
                 ]
isDefaultGenome :: T.Text -> Bool 
isDefaultGenome name = name `elem` (map fst defaultGenomes)

-- | Get download URL for an reference
downloadURL :: FilePath -> IO FilePath
downloadURL genome = case lookup (T.pack genome) defaultGenomes of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> do
            baseURL <- nglessDataBaseURL
            return (baseURL </> v <.> "tar.gz")


getGenomeRootPath :: T.Text -> FilePath
getGenomeRootPath d = 
   case lookup d defaultGenomes of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> v

 
getIndexPath :: FilePath -> FilePath
getIndexPath gen = getGenomeRootPath (T.pack gen) </> bwaIndexPath </> "genome.fa.gz"


