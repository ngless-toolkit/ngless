module Data.DefaultValues
    ( 
    defaultDir
    , htmlDefaultDirLibs
    , htmlDefaultFonts
    , htmlDefaultDir
    , getBWAPath
    , getSAMPath
    , mapAlg
    , samAlg
    , getNglessRoot
    , maxTempFileSize
    ) where

import System.Directory
import System.FilePath.Posix

import System.Environment (getExecutablePath)


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

-- this retrieves the actual path from the symLink
getNglessRoot :: IO FilePath
getNglessRoot = getExecutablePath >>= return . takeDirectory

getBWAPath :: IO String
getBWAPath = getNglessRoot >>= return . (</> bwaDirPath)

getSAMPath :: IO String
getSAMPath = getNglessRoot >>= return . (</> samDirPath)

defaultDir :: IO String
defaultDir = do 
  tdir <- getTemporaryDirectory
  return $ tdir </> "ngless.outputs/"


maxTempFileSize :: Num a => a
maxTempFileSize = 100*1000*1000 -- 100MB

