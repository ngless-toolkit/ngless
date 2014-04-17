{-# LANGUAGE OverloadedStrings #-}

module UnpackIlluminaGenomes
    ( 
      unpack,
      defaultGenomes,
      getUcscUrl,
      bwaGenomePath,
      defGenomeDir,
      getGenomeRootPath,
      getIndexPath,
      getGenomeDirName,
      getGenRootPathFromGenName
    ) where

import System.FilePath( (</>), (<.>) )

import qualified System.FilePath as FP (takeDirectory)
import System.Directory(createDirectoryIfMissing)
import Control.Exception(Exception)

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS

import qualified Data.Map as Map

import qualified Data.Text as T


bwaGenomePath :: FilePath
bwaGenomePath = "Sequence/WholeGenomeFasta"

bwaIndexPath :: FilePath
bwaIndexPath = "Sequence/BWAIndex/version0.6.0"

defGenomeDir :: FilePath
defGenomeDir = "../share/ngless/genomes"

genomesRep :: FilePath
genomesRep = "UCSC"

ucscUrl :: FilePath
ucscUrl = "http://kdbio.inesc-id.pt/~prrm/genomes"

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

getUcscUrl :: FilePath -> FilePath
getUcscUrl genome = do
    let genomeMap = Map.fromList defaultGenomes
        i = Map.lookupIndex (T.pack genome) genomeMap
    case i of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just index -> do
            let res =  Map.elemAt index genomeMap 
            ucscUrl </>  (getGenomeDirName res) <.> "tar.gz"

getGenomeDirName :: (T.Text, FilePath) -> FilePath
getGenomeDirName (a,d) = d ++ "_" ++ genomesRep ++ "_" ++ (T.unpack a)

getGenomeRootPath :: (T.Text, FilePath) -> FilePath
getGenomeRootPath (a,d) = d </> genomesRep </> (T.unpack a)

getGenRootPathFromGenName :: FilePath -> FilePath
getGenRootPathFromGenName fp = 
    case Map.lookupIndex (T.pack fp) mapGens of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just index -> do
            let res =  Map.elemAt index mapGens 
            getGenomeRootPath res
    where
        mapGens = Map.fromList defaultGenomes  

getIndexPath :: FilePath -> FilePath
getIndexPath gen = do
    case Map.lookupIndex (T.pack gen) mapGens of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just index -> do
            let res =  Map.elemAt index mapGens 
            getGenomeDirName res </> getGenomeRootPath res </> bwaIndexPath </> "genome.fa"
    where
        mapGens = Map.fromList defaultGenomes


unpack :: Exception e => FilePath -> Tar.Entries e -> IO ()
unpack baseDir entries = unpackEntries entries
  where
    unpackEntries (Tar.Fail err)      = error ("Error on entry " ++ (show err))
    unpackEntries Tar.Done            = return ()
    unpackEntries (Tar.Next entry es) = do
      case Tar.entryContent entry of
          Tar.NormalFile file _ -> extractFile path file >> unpackEntries es
          Tar.Directory         -> extractDir path >> unpackEntries es
          _                     -> unpackEntries es --ignore other file types
      where
        path = Tar.entryPath entry

    extractFile path content = do
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      where
        absDir  = baseDir </> FP.takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing True (baseDir </> path)


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
