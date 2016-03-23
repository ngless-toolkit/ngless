{-# LANGUAGE OverloadedStrings #-}

module ReferenceDatabases
    ( Reference(..)
    , getBuiltinReference
    , buildGenomePath
    , buildGFFPath
    , createReferencePack
    , ensureDataPresent
    , installData
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import System.FilePath.Posix
import System.Directory
import System.IO.Error
import Data.Maybe
import Data.List        (find)

import Control.Monad
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource(release)

import Utils.Network (downloadFile, downloadOrCopyFile)
import Utils.Bwa as Bwa
import FileManagement (createTempDir)
import Configuration
import Output
import NGLess

data Reference = Reference
    { refName :: T.Text
    , refUrl :: Maybe FilePath
    , refHasGff :: Bool
    , refHasFunctionalMap :: Bool
    } deriving (Eq, Show)

defaultGenomes :: [Reference]
defaultGenomes = [Reference rn Nothing True False | rn <-
                [ "hg19" -- Homo_sapiens
                , "mm10" -- Mus_musculus
                , "rn4" --  Rattus_norvegicus
                , "bosTau4" --  Bos_taurus
                , "canFam2" --Canis_familiaris
                , "dm3" --Drosophila_melanogaster
                , "ce10" --Caenorhabditis_elegans
                , "sacCer3" --Saccharomyces_cerevisiae
                ]]

getBuiltinReference :: T.Text -> Maybe Reference
getBuiltinReference rn = find ((==rn) . refName) defaultGenomes

isDefaultReference = isJust . getBuiltinReference

genomePATH = "Sequence/BWAIndex/genome.fa.gz"
gffPATH = "Annotation/annotation.gtf.gz"

buildGenomePath :: FilePath -> FilePath
buildGenomePath = (</> genomePATH)
buildGFFPath :: FilePath -> FilePath
buildGFFPath = (</> gffPATH)


createReferencePack :: FilePath -> FilePath -> FilePath -> NGLessIO ()
createReferencePack oname genome gtf = do
    (rk,tmpdir) <- createTempDir "ngless_ref_creator_"
    outputListLno' DebugOutput ["Working with temporary directory: ", tmpdir]
    liftIO $ do
        createDirectoryIfMissing True (tmpdir ++ "/Sequence/BWAIndex/")
        createDirectoryIfMissing True (tmpdir ++ "/Annotation/")
        downloadOrCopyFile genome (buildGenomePath tmpdir)
        downloadOrCopyFile gtf (buildGFFPath tmpdir)
    Bwa.createIndex (buildGenomePath tmpdir)
    let filelist = gffPATH:[genomePATH ++ ext | ext <- [""
                                        ,".amb"
                                        ,".ann"
                                        ,".bwt"
                                        ,".pac"
                                        ,".sa"]]
    liftIO $ Tar.create oname tmpdir filelist
    outputListLno' ResultOutput ["Created reference package in file ", oname]
    release rk


downloadReference :: Reference  -> FilePath -> NGLessIO ()
downloadReference ref destPath = do
    url <- case refUrl ref of
        Just u -> return u
        Nothing
            | isDefaultReference (refName ref) -> do
                    baseURL <- nglessDataBaseURL
                    return $ baseURL </> T.unpack (refName ref) <.> "tar.gz"
            | otherwise ->
                throwScriptError ("Expected reference data, got "++T.unpack (refName ref))
    outputListLno' InfoOutput ["Starting download from ", url]
    liftIO $ downloadFile url destPath
    outputLno' InfoOutput "Reference download completed!"


-- | Make sure that reference data is present, downloading it if necessary.
-- Returns the base path for the data.
ensureDataPresent :: String -> NGLessIO FilePath
ensureDataPresent ref = do
    p <- findDataFiles ref
    case p of
        Just p' -> return p'
        Nothing -> installData Nothing ref

-- | Installs reference data uncondictionally
-- When mode is Nothing, tries to install them in global directory, otherwise
-- installs it in the user directory
installData :: Maybe InstallMode -> String -> NGLessIO FilePath
installData Nothing refname = do
    p' <- globalDataDirectory
    canInstallGlobal <- liftIO $ do
        created <- (createDirectoryIfMissing False p' >> return True) `catchIOError` (\_ -> return False)
        if created
            then writable <$> getPermissions p'
            else return False
    if canInstallGlobal
        then installData (Just Root) refname
        else installData (Just User) refname
installData (Just mode) refname = do
    basedir <- if mode == Root
                then globalDataDirectory
                else userDataDirectory
    liftIO $ createDirectoryIfMissing True basedir
    let tarName = basedir </> refname <.> "tar.gz"
    ref <- case getBuiltinReference (T.pack refname) of
        Just r -> return r
        Nothing -> throwScriptError ("Cannot install unknown reference '"++refname)
    downloadReference ref tarName
    liftIO $
        Tar.unpack basedir . Tar.read . GZip.decompress =<< BL.readFile tarName
    return (basedir </> refname)



-- | checks first user and then global data directories for <ref>.
-- The user directory is checked first to allow the user to override a
-- problematic global installation.
findDataFiles :: FilePath -> NGLessIO (Maybe FilePath)
findDataFiles ref = liftM2 (<|>)
    (findDataFilesIn ref User)
    (findDataFilesIn ref Root)

findDataFilesIn :: FilePath -> InstallMode -> NGLessIO (Maybe FilePath)
findDataFilesIn ref mode = do
    basedir <- if mode == Root
                    then globalDataDirectory
                    else userDataDirectory
    let refdir = basedir </> ref
    hasIndex <- hasValidIndex (buildGenomePath refdir)
    outputListLno' TraceOutput ["Looked for ", ref, " in directory ", refdir, if hasIndex then " (and found it)" else " (and did not find it)"]
    return (if hasIndex
                then Just refdir
                else Nothing)

