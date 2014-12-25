module Utils.Tempfile
    ( tempfile
    ) where

import System.Directory
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad.Trans.Resource

import Configuration (temporaryFileDirectory)


removeFileIfExists fp = removeFile fp `catch` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e


tempfile :: MonadResource m => FilePath -> m (ReleaseKey, (FilePath,Handle))
tempfile template = allocate getTemp removeTemp
    where
        getTemp = do
            dir <- temporaryFileDirectory
            openTempFile dir template
        removeTemp (fp, h) = hClose h >> removeFileIfExists fp
