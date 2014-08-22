module Data.DefaultValues
    ( 
    defaultDir
    , htmlDefaultDirLibs
    , htmlDefaultFonts
    , htmlDefaultDir
    , numFiles
    , getBWAPath
    , getSAMPath
    , indexRequiredFormats
    , mapAlg
    , samAlg
    , defGenomeDir
    , suGenomeDir
    , getNglessRoot
    , switchToCurDirectory
    , InstallMode(..)
    ) where

import System.Directory
import System.FilePath.Posix
import System.IO

import System.Environment (getExecutablePath)


data InstallMode = User
                | Root
            deriving (Eq, Show)


-- relative paths 

htmlDefaultDirLibs :: String
htmlDefaultDirLibs = "htmllibs"

htmlDefaultFonts :: String
htmlDefaultFonts = "fonts"

htmlDefaultDir :: IO FilePath
htmlDefaultDir = getNglessRoot >>= return . (</> "../share/ngless/Html")

samDirPath :: String
samDirPath = "../share/ngless/samtools-1.0" --setup puts the samtools directory on project root.

samAlg :: String
samAlg = "samtools"

bwaDirPath :: String
bwaDirPath = "../share/ngless/bwa-0.7.7" --setup puts the bwa directory on project root.

mapAlg :: String
mapAlg = "bwa"

indexRequiredFormats :: [String]
indexRequiredFormats = [".amb",".ann",".bwt",".pac",".sa"]

defGenomeDir :: IO FilePath
defGenomeDir = getHomeDirectory >>= \x -> return ( x </> ".ngless/genomes" )


-- used to install genomes across all users
suGenomeDir :: FilePath
suGenomeDir = "../share/ngless/genomes"


-- this retrieves the actual path from the symLink
getNglessRoot :: IO FilePath
getNglessRoot = getExecutablePath >>= return . takeDirectory

getBWAPath :: IO String
getBWAPath = getNglessRoot >>= return . (</> bwaDirPath)

getSAMPath :: IO String
getSAMPath = getNglessRoot >>= return . (</> samDirPath)

switchToCurDirectory :: IO ()
switchToCurDirectory = getCurrentDirectory >>= setCurrentDirectory

--- getExecutablePath
-- Returns for example: /usr/local/bin/ngless
-- Symbolic link to deps at /usr/local/opt/ngless
-- from exec to deps is "../../opt/ngless/Html"
--- 

defaultDir :: IO String
defaultDir = do 
  tdir <- getTemporaryDirectory
  return $ tdir </> "ngless.outputs/"


maxFileSize :: Num a => a
maxFileSize = 100*1000*1000 -- 100MB

calcSize :: Integer -> Integer
calcSize s = ceiling $ ((fromInteger s) / maxFileSize :: Double) 

numFiles :: FilePath -> IO Integer
numFiles path = do
    size' <- getFileSize path
    return $ calcSize size'
 where
   getFileSize p = withFile p ReadMode hFileSize

