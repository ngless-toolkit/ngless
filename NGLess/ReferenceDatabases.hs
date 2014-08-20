{-# LANGUAGE OverloadedStrings #-}

module ReferenceDatabases
    ( 
      defaultGenomes,
      getUcscUrl,
      getIndexPath,
      getGff,
      getGenomeRootPath,
      isDefaultGenome
    ) where

import qualified Data.Text as T
import System.FilePath( (</>), (<.>) )

--bwaGenomePath :: FilePath
--bwaGenomePath = "Sequence/BWAIndex/genome.fa"

bwaIndexPath :: FilePath
bwaIndexPath = "Sequence/BWAIndex"

gffPath :: FilePath
gffPath = "Annotation/annot.gtf.gz"

getGff :: T.Text -> FilePath
getGff n = (T.unpack n) </> gffPath

nglessDataBaseURL :: FilePath
nglessDataBaseURL = "http://kdbio.inesc-id.pt/~prrm/genomes"

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

getUcscUrl :: FilePath -> FilePath
getUcscUrl genome = 
    case lookup (T.pack genome) defaultGenomes of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> nglessDataBaseURL </> v <.> "tar.gz"


getGenomeRootPath :: T.Text -> FilePath
getGenomeRootPath d = 
   case lookup d defaultGenomes of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> v

 
getIndexPath :: FilePath -> FilePath
getIndexPath gen = getGenomeRootPath (T.pack gen) </> bwaIndexPath </> "genome.fa.gz"



--import qualified Data.ByteString.Lazy as BS
--import qualified System.FilePath as FP (takeDirectory)
--import System.Directory(createDirectoryIfMissing)
--import Control.Exception(Exception)

--import qualified Codec.Archive.Tar       as Tar
--import qualified Codec.Archive.Tar.Entry as Tar
--import qualified Data.Map as Map


--unpack :: Exception e => FilePath -> Tar.Entries e -> IO ()
--unpack baseDir entries = unpackEntries entries
--  where
--    unpackEntries (Tar.Fail err)      = error ("Error on entry " ++ (show err))
--    unpackEntries Tar.Done            = return ()
--    unpackEntries (Tar.Next entry es) = do
--      case Tar.entryContent entry of
--          Tar.NormalFile file _ -> print path >> extractFile path file >> unpackEntries es
--          Tar.Directory         -> print path >> extractDir path >> unpackEntries es
--          _                     -> print path >> unpackEntries es --ignore other file types
--      where
--        path = Tar.fromTarPathToPosixPath $ Tar.entryTarPath entry

--    extractFile path content = do
--      createDirectoryIfMissing True absDir
--      BS.writeFile absPath content
--      where
--        absDir  = baseDir </> FP.takeDirectory path
--        absPath = baseDir </> path

--    extractDir path = createDirectoryIfMissing True (baseDir </> path)


-- bwa does not work with symlinks
--createSymLinks :: FilePath -> FilePath -> IO ()
--createSymLinks dir ref = do
--  files <- getFilesInDir origDir'
--  mapM_ (\fOrig -> createSymbolicLink fOrig (destDir' </> FP.takeFileName fOrig)) files
--  createSymbolicLink (seqDir </> "WholeGenomeFasta/genome.fa") (bwaDir </> "genome.fa") 
--  where 
--    seqDir = dir </> getGenRootPathFromGenName ref </> "Sequence"
--    bwaDir = seqDir </> "BWAIndex"
--    origDir' = bwaDir </> "version0.6.0"
--    destDir' = origDir' </> "../"
