
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
import Control.Exception

import FileManagement

dirPath = "../bwa-0.7.7/" --setup puts the bwa directory on project root.
mapAlg = "bwa"


indexReference refPath = readProcess (dirPath </> mapAlg) ["index", (T.unpack refPath)] ""


mapToReference refIndex readSet = do
	newfp <- getTempFilePath readSet
	putStrLn $ "write .sam file to: " ++ (show newfp)
	jHandle <- mapToReference' newfp refIndex readSet
	exitCode <- waitForProcess jHandle
	case exitCode of
		ExitSuccess -> return ()
		ExitFailure err -> error ("Failure on mapping against reference:" ++ (show err))


-- Process to execute BWA and write to h .sam file
mapToReference' newfp refIndex readSet = 
		let ls h = runProcess (dirPath </> mapAlg)    -- Executable location
	                          ["mem","-t",(show numCapabilities), (T.unpack refIndex), readSet] -- Parameters
	                          Nothing      -- Working directory: current dir
	                          Nothing      -- Standard environment
	                          Nothing      -- STDIN
	                          (Just h)     -- Connect STDOUT to the Handle opened above
	                          Nothing      -- STDERR
	    in bracket (openFile newfp WriteMode) hClose ls
