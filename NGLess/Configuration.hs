module Configuration
    ( nglessDataBaseURL
    , InstallMode(..)
    , globalDataDirectory
    , userDataDirectory
    , printNglessLn
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
