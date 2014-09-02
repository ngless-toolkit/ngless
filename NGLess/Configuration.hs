module Configuration
    ( nglessDataBaseURL
    , InstallMode(..)
    , globalDataDirectory
    , userDataDirectory
    , printNglessLn
    , getNglessRoot
    , samtoolsBin
    , bwaBin
    , outputDirectory

    , htmlDefaultDir
    , maxTempFileSize
    ) where

import Control.Monad (unless)
import Control.Applicative ((<$>))
import System.Environment (getExecutablePath)
import System.Directory
import System.FilePath.Posix
import System.Console.CmdArgs.Verbosity (whenLoud)


data InstallMode = User | Root deriving (Eq, Show)

nglessDataBaseURL :: IO FilePath
nglessDataBaseURL = return "http://kdbio.inesc-id.pt/~prrm/genomes"

globalDataDirectory :: IO FilePath
globalDataDirectory = do
    base <- takeDirectory <$> getExecutablePath
    let dir = base </> "../share/ngless/data"
    return dir

userDataDirectory :: IO FilePath
userDataDirectory = (</> ".ngless/data") <$> getHomeDirectory

printNglessLn :: String -> IO ()
printNglessLn msg = whenLoud (putStrLn msg)


getNglessRoot :: IO FilePath
getNglessRoot = takeDirectory <$> getExecutablePath

check_executable name bin = do
    exists <- doesFileExist bin
    unless exists
        (error $ concat [name, " binary not found!\n","Expected it at ", bin])
    is_executable <- executable <$> getPermissions bin
    unless is_executable
        (error $ concat [name, " binary found at ", bin, ".\nHowever, it is not an executable file!"])
    return bin

bwaBin :: IO FilePath
bwaBin = do
    bin <- (</> bwaDirPath </> "bwa") <$> getNglessRoot
    check_executable "BWA" bin

bwaDirPath :: String
bwaDirPath = "../share/ngless/bwa-0.7.7" --setup puts the bwa directory on project root.

samDirPath :: String
samDirPath = "../share/ngless/samtools-1.0"
samtoolsBin :: IO FilePath
samtoolsBin = do
    bin <- (</> samDirPath </> "samtools") <$> getNglessRoot
    check_executable "samtools" bin


htmlDefaultDir :: IO FilePath
htmlDefaultDir = (</> "../share/ngless/Html") <$> getNglessRoot

outputDirectory :: IO String
outputDirectory = do
  tdir <- getTemporaryDirectory
  return $ tdir </> "ngless.outputs/"


maxTempFileSize :: Num a => IO a
maxTempFileSize = return (100*1000*1000) -- 100MB
