{-# LANGUAGE OverloadedStrings #-}

module ReferenceDatabases
    ( isDefaultReference
    , configGenome
    , defaultGenomes
    , getGenomeRootPath
    , getIndexPath
    , getGff
    , downloadReference
    , indexRequiredFormats
    , ensureDataPresent
    , doAllFilesExist
    , findIndexFiles
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip


import System.FilePath.Posix
import System.Directory
import Control.Monad
import Data.Maybe

import System.IO.Error

import Control.Applicative ((<$>))

import Utils.Network
import Configuration

indexRequiredFormats :: [String]
indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]


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
isDefaultReference :: T.Text -> Bool
isDefaultReference name = name `elem` (map fst defaultGenomes)

-- | Get download URL for a reference
downloadURL :: FilePath -> IO FilePath
downloadURL genome = case lookup (T.pack genome) defaultGenomes of
        Nothing -> error ("Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> do
            baseURL <- nglessDataBaseURL
            return (baseURL </> v <.> "tar.gz")


getGenomeRootPath :: T.Text -> FilePath
getGenomeRootPath d = case lookup d defaultGenomes of
        Nothing -> error ("getGenomeRootPath: Should be a valid genome. The available genomes are " ++ (show defaultGenomes))
        Just v -> v

getIndexPath :: FilePath -> FilePath
getIndexPath gen = getGenomeRootPath (T.pack gen) </> bwaIndexPath </> "genome.fa.gz"

downloadReference :: String -> FilePath -> IO ()
downloadReference ref destPath = do
    url <- downloadURL ref
    downloadFile url destPath
    putStrLn " Reference download completed! "

ensureDataPresent :: String -> IO FilePath
ensureDataPresent ref = do
    p' <- globalDataDirectory
    created <- (createDirectoryIfMissing False p' >> return True) `catchIOError` (\_ -> return False)
    canInstallGlobal <- (if created
                    then writable <$> getPermissions p' -- check whether can write globally
                    else return False)
    if canInstallGlobal
        then configGenome Root ref
        else do
            udir <- userDataDirectory
            createDirectoryIfMissing True udir
            configGenome User ref

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

configGenome :: InstallMode -> String -> IO FilePath
configGenome mode ref = do
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
    Tar.unpack d . Tar.read . GZip.decompress =<< BL.readFile ( d </> tarName)
   where
        dirName = case lookup (T.pack ref) defaultGenomes of
            Nothing -> error ("Should be a valid genome. The available genomes are " ++ show defaultGenomes)
            Just v  -> v
        tarName = dirName <.> "tar.gz"

doAllFilesExist :: String -> [String] -> IO Bool
doAllFilesExist _ [] = return True
doAllFilesExist basepath (x:xs) = do
    isThere <- doesFileExist (basepath ++ x)
    if isThere
        then doAllFilesExist basepath xs
        else return False

