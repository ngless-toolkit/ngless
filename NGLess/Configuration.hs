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
    , temporaryFileDirectory
    , versionStr
    ) where

import Control.Monad (unless)
import Control.Applicative ((<$>))
import System.Environment (getExecutablePath)
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString as B
import Data.Maybe

import Dependencies.Embedded

data InstallMode = User | Root deriving (Eq, Show)

versionStr :: String
versionStr = "0.0.0"

nglessDataBaseURL :: IO FilePath
nglessDataBaseURL = return "http://127.0.0.1/"

globalDataDirectory :: IO FilePath
globalDataDirectory = (</> "../share/ngless/data") <$> getNglessRoot

userNglessDirectory :: IO FilePath
userNglessDirectory = (</> ".ngless") <$> getHomeDirectory

userDataDirectory :: IO FilePath
userDataDirectory = (</> "data") <$> userNglessDirectory

getNglessRoot :: IO FilePath
getNglessRoot = takeDirectory <$> getExecutablePath

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


binPath :: InstallMode -> IO FilePath
binPath Root = (</> "bin") <$> getNglessRoot
binPath User = (</> "bin") <$> userNglessDirectory

findBin :: FilePath -> IO (Maybe FilePath)
findBin fname = do
    rootPath <- (</> fname) <$> binPath Root
    rootex <- canExecute rootPath
    if rootex then
        return (Just rootPath)
    else do
        userpath <- (</> fname) <$> binPath User
        userer <- canExecute userpath
        if userer
            then return (Just userpath)
            else return Nothing

writeBin :: FilePath -> B.ByteString -> IO FilePath
writeBin fname bindata = do
    userBinPath <- binPath User
    createDirectoryIfMissing True userBinPath
    let fname' = userBinPath </> fname
    B.writeFile fname' bindata
    p <- getPermissions fname'
    setPermissions fname' (setOwnerExecutable True p)
    return fname'

findOrCreateBin :: FilePath -> B.ByteString -> IO FilePath
findOrCreateBin fname bindata = do
    path <- findBin fname
    if isJust path
        then return (fromJust path)
        else writeBin fname bindata

bwaBin :: IO FilePath
bwaBin = findOrCreateBin bwaFname =<< bwaData
    where
        bwaFname = ("ngless-" ++ versionStr ++ "-bwa")

samtoolsBin :: IO FilePath
samtoolsBin = findOrCreateBin samtoolsFname =<< samtoolsData
    where
        samtoolsFname = ("ngless-" ++ versionStr ++ "-samtools")


outputDirectory :: FilePath -> IO FilePath
outputDirectory ifile = return $ replaceExtension ifile ".output_ngless/"

temporaryFileDirectory :: IO FilePath
temporaryFileDirectory = getTemporaryDirectory -- in the future this will be configurable
