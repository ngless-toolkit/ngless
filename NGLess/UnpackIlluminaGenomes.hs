module UnpackIlluminaGenomes
    ( 
      unpack
    ) where

import System.FilePath( (</>) )

import qualified System.FilePath as FilePath.Native (takeDirectory)
import System.Directory(createDirectoryIfMissing)
import Control.Exception(Exception)

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS

unpack :: Exception e => FilePath -> Tar.Entries e -> IO ()
unpack baseDir entries = unpackEntries entries
  where
    unpackEntries (Tar.Fail err)      = error ("Error on entry" ++ (show err))
    unpackEntries Tar.Done            = return ()
    unpackEntries (Tar.Next entry es) = do
      case Tar.entryContent entry of
          Tar.NormalFile file _ -> extractFile path file >> unpackEntries es
          Tar.Directory         -> extractDir path >> unpackEntries es
          _                     -> unpackEntries es --ignore other file types
      where
        path = Tar.entryPath entry

    extractFile path content = do
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing True (baseDir </> path)

createSymLinks :: FilePath -> IO ()
createSymLinks fp = return ()