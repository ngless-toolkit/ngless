{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
module Configuration
    ( nglessDataBaseURL
    , InstallMode(..)
    , globalDataDirectory
    , userDataDirectory
    , samtoolsBin
    , bwaBin
    , outputDirectory
    , setOutputDirectory
    , temporaryFileDirectory
    , setTemporaryDirectory
    , setKeepTemporaryFiles
    , setTraceFlag
    , traceFlag
    , versionStr
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import System.Environment (getExecutablePath)
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString as B
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import NGLess
import Dependencies.Embedded

data InstallMode = User | Root deriving (Eq, Show)

versionStr :: String
versionStr = "0.0.0"

nglessDataBaseURL :: NGLessIO FilePath
nglessDataBaseURL = return "http://127.0.0.1/"

globalDataDirectory :: NGLessIO FilePath
globalDataDirectory = (</> "../share/ngless/data") <$> getNglessRoot

userNglessDirectory :: NGLessIO FilePath
userNglessDirectory = (</> ".ngless") <$> (liftIO getHomeDirectory)

userDataDirectory :: NGLessIO FilePath
userDataDirectory = (</> "data") <$> userNglessDirectory

getNglessRoot :: NGLessIO FilePath
getNglessRoot = takeDirectory <$> (liftIO getExecutablePath)

check_executable :: String -> FilePath -> IO FilePath
check_executable name bin = do
    exists <- doesFileExist bin
    unless exists
        (error $ concat [name, " binary not found!\n","Expected it at ", bin])
    is_executable <- executable <$> getPermissions bin
    unless is_executable
        (error $ concat [name, " binary found at ", bin, ".\nHowever, it is not an executable file!"])
    return bin

canExecute bin = do
    exists <- doesFileExist bin
    if exists
        then executable <$> getPermissions bin
        else return False


binPath :: InstallMode -> NGLessIO FilePath
binPath Root = (</> "bin") <$> getNglessRoot
binPath User = (</> "bin") <$> userNglessDirectory

findBin :: FilePath -> NGLessIO (Maybe FilePath)
findBin fname = do
    rootPath <- (</> fname) <$> binPath Root
    rootex <- liftIO $ canExecute rootPath
    if rootex then
        return (Just rootPath)
    else do
        userpath <- (</> fname) <$> binPath User
        userex <- liftIO $ canExecute userpath
        if userex
            then return (Just userpath)
            else return Nothing

writeBin :: FilePath -> B.ByteString -> NGLessIO FilePath
writeBin fname bindata = do
    userBinPath <- binPath User
    liftIO $ do
        createDirectoryIfMissing True userBinPath
        let fname' = userBinPath </> fname
        B.writeFile fname' bindata
        p <- getPermissions fname'
        setPermissions fname' (setOwnerExecutable True p)
        return fname'

findOrCreateBin :: FilePath -> B.ByteString -> NGLessIO FilePath
findOrCreateBin fname bindata = do
    path <- findBin fname
    if isJust path
        then return (fromJust path)
        else writeBin fname bindata

bwaBin :: NGLessIO FilePath
bwaBin = findOrCreateBin bwaFname =<< liftIO bwaData
    where
        bwaFname = ("ngless-" ++ versionStr ++ "-bwa")

samtoolsBin :: NGLessIO FilePath
samtoolsBin = findOrCreateBin samtoolsFname =<< liftIO samtoolsData
    where
        samtoolsFname = ("ngless-" ++ versionStr ++ "-samtools")


outputDirectoryRef :: IORef FilePath
{-# NOINLINE outputDirectoryRef #-}
outputDirectoryRef = unsafePerformIO (newIORef "")

setOutputDirectory :: FilePath -> IO ()
setOutputDirectory = writeIORef outputDirectoryRef

outputDirectory :: NGLessIO FilePath
outputDirectory = liftIO $ readIORef outputDirectoryRef

temporaryDirectoryRef :: IORef (Maybe FilePath)
{-# NOINLINE temporaryDirectoryRef #-}
temporaryDirectoryRef = unsafePerformIO (newIORef Nothing)

setTemporaryDirectory :: Maybe FilePath -> IO ()
setTemporaryDirectory = writeIORef temporaryDirectoryRef

temporaryFileDirectory :: NGLessIO FilePath
temporaryFileDirectory = liftIO $ do
    tdir <- readIORef temporaryDirectoryRef
    case tdir of
        Just t -> return t
        Nothing -> getTemporaryDirectory

setKeepTemporaryFiles :: Bool -> IO ()
setKeepTemporaryFiles _ = return ()

traceRef :: IORef Bool
{-# NOINLINE traceRef #-}
traceRef = unsafePerformIO (newIORef False)

setTraceFlag :: Bool -> IO ()
setTraceFlag = writeIORef traceRef

traceFlag :: NGLessIO Bool
traceFlag = liftIO $ readIORef traceRef

