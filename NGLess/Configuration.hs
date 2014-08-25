module Configuration
    ( nglessDataBaseURL
    , InstallMode(..)
    ) where

data InstallMode = User | Root deriving (Eq, Show)

nglessDataBaseURL :: IO FilePath
nglessDataBaseURL = return "http://kdbio.inesc-id.pt/~prrm/genomes"
