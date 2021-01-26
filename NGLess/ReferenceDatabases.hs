{- Copyright 2013-2021 NGLess Authors
 - License: MIT
-}
module ReferenceDatabases
    ( Reference(..)
    , ReferenceFilePaths(..)
    , builtinReferences
    , isBuiltinReference
    , createReferencePack
    , ensureDataPresent
    , installData
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Monad.Extra (whenJust, unlessM)

import System.FilePath
import System.Directory
import System.IO.Error
import Data.Foldable
import Data.Maybe

import Control.Monad (liftM2)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource(release)

import qualified StandardModules.Mappers.Bwa as Bwa
import FileManagement (createTempDir, InstallMode(..))
import NGLess.NGLEnvironment (NGLVersion(..), NGLEnvironment(..), nglConfiguration, nglEnvironment)
import Configuration
import Utils.Network (downloadExpandTar, downloadOrCopyFile, downloadFile, isUrl)
import Utils.LockFile (withLockFile, LockParameters(..), WhenExistsStrategy(..))
import Utils.Utils
import Modules
import Version (versionStr)
import Output (outputListLno', OutputType(..))
import NGLess

data ReferenceFilePaths = ReferenceFilePaths
    { rfpFaFile :: Maybe FilePath
    , rfpGffFile :: Maybe FilePath
    , rfpFunctionalMap :: Maybe FilePath
    } deriving (Eq, Show)


dataDirectory :: InstallMode -> NGLessIO FilePath
dataDirectory Root = nConfGlobalDataDirectory <$> nglConfiguration
dataDirectory User = nConfUserDataDirectory   <$> nglConfiguration

-- These references were obtained from Ensembl
-- Check build-scripts/create-standard-packs.py for additional information
builtinReferences :: [Reference]
builtinReferences = [Reference rn (Just alias) (T.concat [rn, "-v", ver, "-", T.pack versionStr]) Nothing True False | (rn, alias, ver) <-
                [ ("bosTau4", "Bos_taurus_UMD3.1", "1.0")
                , ("ce10", "Caenorhabditis_elegans_WBcel235", "1.0")
                , ("canFam3", "Canis_familiaris_CanFam3.1", "1.0")
                , ("dm5", "Drosophila_melanogaster_BDGP5", "1.0")
                , ("dm6", "Drosophila_melanogaster_BDGP6", "1.0")
                , ("gg4", "Gallus_gallus_Galgal4", "1.0")
                , ("gg5", "Gallus_gallus_5.0", "1.0")
                , ("hg19", "Homo_sapiens_GRCh37.p13", "1.0")
                , ("hg38.p7", "Homo_sapiens_GRCh38.p7", "1.0")
                , ("hg38.p10", "Homo_sapiens_GRCh38.p10", "1.0")
                , ("mm10.p2", "Mus_musculus_GRCm38.p2", "1.0")
                , ("mm10.p5", "Mus_musculus_GRCm38.p5", "1.0")
                , ("rn5", "Rattus_norvegicus_Rnor_5.0", "1.0")
                , ("rn6", "Rattus_norvegicus_Rnor_6.0", "1.0")
                , ("sacCer3", "Saccharomyces_cerevisiae_R64-1-1", "1.0")
                , ("susScr11", "Sus_scrofa.Sscrofa11.1", "1.0")
                ]]


findReference :: [Reference] -> T.Text -> Maybe Reference
findReference allrefs rn = find (\ref -> (refName ref == rn) || maybe False (== rn) (refAlias ref)) allrefs

-- Is this a builtin reference (i.e., can be used without importing a module)?
isBuiltinReference :: T.Text -> Bool
isBuiltinReference rn = isJust $ findReference builtinReferences rn

-- Download if it's a URL
downloadIfUrl :: FilePath -- ^ base directory
                -> FilePath -- ^ local filename
                -> Maybe FilePath
                -> NGLessIO (Maybe FilePath)
downloadIfUrl _ _ Nothing = return Nothing
downloadIfUrl basedir fname (Just path)
    | isUrl path = do
        let local = basedir </> "cached" </> fname
        liftIO $ createDirectoryIfMissing True (basedir </> "cached")
        unlessM (liftIO $ doesFileExist local) $
            withLockFile LockParameters
                     { lockFname = local ++ ".download.lock"
                     , maxAge = 300
                     , whenExistsStrategy = IfLockedRetry { nrLockRetries = 37*60, timeBetweenRetries = 60 }
                     , mtimeUpdate = True
                    } $ do
                -- recheck with lock
                unlessM (liftIO $ doesFileExist local) $
                    downloadFile path local
        return (Just local)
    | otherwise = return (Just path)

moduleDirectReference :: T.Text -> NGLessIO (Maybe ReferenceFilePaths)
moduleDirectReference rname = do
    mods <- loadedModules
    findM mods $ \m ->
        findM (modReferences m) $ \case
            ExternalReference eref fafile gtffile mapfile
                | eref == rname -> do
                    fafile'  <- downloadIfUrl (modPath m) (T.unpack rname <.> "fna.gz") (Just fafile)
                    gtffile' <- downloadIfUrl (modPath m) (T.unpack rname <.> "gff.gz") gtffile
                    mapfile' <- downloadIfUrl (modPath m) (T.unpack rname <.> "tsv.gz") mapfile
                    return . Just $! ReferenceFilePaths fafile' gtffile' mapfile'
            _ -> return Nothing

referencePath = "Sequence/BWAIndex/reference.fa.gz"
gffPath = "Annotation/annotation.gtf.gz"
functionalMapPath = "Annotation/functional.map.gz"

buildFaFilePath :: FilePath -> FilePath
buildFaFilePath = (</> referencePath)

buildGFFPath :: FilePath -> FilePath
buildGFFPath = (</> gffPath)

buildFunctionalMapPath :: FilePath -> FilePath
buildFunctionalMapPath = (</> functionalMapPath)

-- Create an NGLess-style reference pack, including a FASTA file, and possibly
-- one of two annotation files (GFF or TSV)
createReferencePack :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> NGLessIO ()
createReferencePack oname reference mgtf mfunc = do
    (rk,tmpdir) <- createTempDir "ngless_ref_creator_"
    outputListLno' DebugOutput ["Working with temporary directory: ", tmpdir]
    liftIO $ do
        createDirectoryIfMissing True (tmpdir ++ "/Sequence/BWAIndex/")
        createDirectoryIfMissing True (tmpdir ++ "/Annotation/")
    downloadOrCopyFile reference (buildFaFilePath tmpdir)
    whenJust mgtf $ \gtf ->
        downloadOrCopyFile gtf (buildGFFPath tmpdir)
    whenJust mfunc $ \func ->
        downloadOrCopyFile func (buildFunctionalMapPath tmpdir)
    Bwa.createIndex (buildFaFilePath tmpdir)
    let referencefiles = [referencePath ++ ext |
                                    ext <- [""
                                        ,".amb"
                                        ,".ann"
                                        ,".bwt"
                                        ,".pac"
                                        ,".sa"]]
        filelist = referencefiles ++ [gffPath | isJust mgtf] ++ [functionalMapPath | isJust mfunc]
    liftIO $
        BL.writeFile oname . GZip.compress . Tar.write =<< Tar.pack tmpdir filelist
    outputListLno' ResultOutput ["Created reference package in file ", oname]
    release rk



-- | Make sure that the passed reference is present, downloading it if
-- necessary.
-- Returns the base path for the data.
ensureDataPresent :: T.Text -- ^ reference name (unversioned)
                        -> NGLessIO ReferenceFilePaths
ensureDataPresent rname = moduleDirectReference rname >>= \case
        Just r -> return r
        Nothing -> findDataFiles (T.unpack rname) >>= \case
            Just refdir -> wrapRefDir refdir
            Nothing -> wrapRefDir =<< installData Nothing rname
    where
        wrapRefDir refdir = return $! ReferenceFilePaths
                        (Just $ buildFaFilePath refdir)
                        (Just $ buildGFFPath refdir)
                        Nothing

-- | Installs reference data uncondictionally
-- When mode is Nothing, tries to install them in global directory, otherwise
-- installs it in the user directory
installData :: Maybe InstallMode -> T.Text -> NGLessIO FilePath
installData Nothing refname = do
    p' <- nConfGlobalDataDirectory <$> nglConfiguration
    canInstallGlobal <- liftIO $ do
        created <- (createDirectoryIfMissing False p' >> return True) `catchIOError` (\_ -> return False)
        if created
            then writable <$> getPermissions p'
            else return False
    if canInstallGlobal
        then installData (Just Root) refname
        else installData (Just User) refname
installData (Just mode) refname = do
    basedir <- dataDirectory mode
    mods <- loadedModules
    let unpackRef (ExternalPackagedReference r) = Just r
        unpackRef _ = Nothing
        refs = mapMaybe unpackRef $ concatMap modReferences mods
    ref  <- case findReference (refs ++ builtinReferences) refname of
        Just ref -> return ref
        Nothing -> throwScriptError ("Could not find reference '" ++ T.unpack refname ++ "'. It is not builtin nor in one of the loaded modules.")
    url <- case refUrl ref of
        Just u -> return u
        Nothing
            | isBuiltinReference (refName ref) -> do
                    version@(NGLVersion majV minV) <- ngleVersion <$> nglEnvironment
                    let versionDirectory = if version < NGLVersion 0 9
                                                then ""
                                                else show majV ++ "." ++ show minV
                    baseURL <- nConfDownloadBaseURL <$> nglConfiguration
                    return $ baseURL </> "References" </> versionDirectory </> T.unpack (refName ref) <.> "tar.gz"
            | otherwise ->
                throwScriptError ("Expected reference data, got "++T.unpack (refName ref))
    outputListLno' InfoOutput ["Starting download from ", url]
    let destdir = basedir </> "References" </> T.unpack refname
    downloadExpandTar url destdir
    outputListLno' InfoOutput ["Reference download completed!"]
    return destdir



-- | checks first user and then global data directories for <ref>.
-- The user directory is checked first to allow the user to override a
-- problematic global installation.
findDataFiles :: FilePath -> NGLessIO (Maybe FilePath)
findDataFiles ref = liftM2 (<|>)
    (findDataFilesIn ref User)
    (findDataFilesIn ref Root)

findDataFilesIn :: FilePath -> InstallMode -> NGLessIO (Maybe FilePath)
findDataFilesIn ref mode = do
    basedir <- dataDirectory mode
    let refdir = basedir </> "References" </> ref
    hasIndex <- Bwa.hasValidIndex (buildFaFilePath refdir)
    outputListLno' TraceOutput ["Looked for ", ref, " in directory ", refdir, if hasIndex then " (and found it)" else " (and did not find it)"]
    return $! (if hasIndex
                then Just refdir
                else Nothing)

