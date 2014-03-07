
module InvokeExternalProgs
    ( 
    indexReference,
    mapToReference
    ) where

import GHC.Conc -- Returns number of cores available

import Data.Text as T

import System.FilePath.Posix
import System.Process
import System.Exit

import System.IO

import FileManagement

dirPath = "../bwa-0.7.7/" --setup puts the bwa directory on project root.
mapAlg = "bwa"


indexReference refPath = readProcess (dirPath </> mapAlg) ["index", (T.unpack refPath)] ""

mapToReference refIndex readSet = do
    (_, Just hout, _, jHandle) <-
        createProcess (proc (dirPath </> mapAlg) ["aln","-t",(show numCapabilities), (T.unpack refIndex), readSet])
           { std_out = CreatePipe
           , std_err = CreatePipe 
           }

    newfp <- getTempFilePath readSet
    hGetLine hout >>= writeFile newfp

    exitCode <- waitForProcess jHandle
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure err -> error ("Failure on mapping against reference:" ++ (show err))
