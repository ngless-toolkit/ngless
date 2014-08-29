module Configuration
    ( nglessDataBaseURL
    , InstallMode(..)
    , globalDataDirectory
    , userDataDirectory
    , printNglessLn
    , getNglessRoot
    , samtoolsBin
    , bwaBin

    , defaultDir
    , htmlDefaultDirLibs
    , htmlDefaultFonts
    , htmlDefaultDir
    , maxTempFileSize
    ) where

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

samDirPath :: String
samDirPath = "../share/ngless/samtools-1.0"
samtoolsBin :: IO FilePath
samtoolsBin = (</> samDirPath </> "samtools") <$> getNglessRoot

bwaBin :: IO FilePath
bwaBin = (</> bwaDirPath </> "bwa") <$> getNglessRoot

bwaDirPath :: String
bwaDirPath = "../share/ngless/bwa-0.7.7" --setup puts the bwa directory on project root.

htmlDefaultDirLibs :: String
htmlDefaultDirLibs = "htmllibs"

htmlDefaultFonts :: String
htmlDefaultFonts = "fonts"

htmlDefaultDir :: IO FilePath
htmlDefaultDir = getNglessRoot >>= return . (</> "../share/ngless/Html")

defaultDir :: IO String
defaultDir = do
  tdir <- getTemporaryDirectory
  return $ tdir </> "ngless.outputs/"


maxTempFileSize :: Num a => a
maxTempFileSize = 100*1000*1000 -- 100MB
