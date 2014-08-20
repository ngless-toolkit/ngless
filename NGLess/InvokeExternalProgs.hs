
module InvokeExternalProgs
    ( 
    indexReference,
    mapToReference,
    convertSamToBam,
    indexRequiredFormats
    ) where

import GHC.Conc -- Returns number of cores available

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import System.FilePath.Posix
import System.Process
import System.Exit

import System.IO

import FileManagement
import Data.DefaultValues


indexReference refPath = do
    let refPath' = (T.unpack refPath)
    res <- doesDirContainFormats refPath' indexRequiredFormats
    case res of
        False -> do
            bwaPath <- getBWAPath
            (exitCode, hout, herr) <-
                readProcessWithExitCode (bwaPath </> mapAlg) ["index", refPath'] []  
            printNglessLn herr
            printNglessLn hout
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _err -> error (herr)
        True -> printNglessLn $ "index for " ++ refPath' ++ " as been sucessfully generated."
            -- already contain reference index
    return refPath'



mapToReference refIndex readSet = do
    newfp <- getTempFilePath readSet
    let newfp' = newfp ++ ".sam"
    printNglessLn $ "write .sam file to: " ++ (show newfp')
    jHandle <- mapToReference' newfp' refIndex readSet
    exitCode <- waitForProcess jHandle
    case exitCode of
       ExitSuccess -> return newfp'
       ExitFailure err -> error ("Failure on mapping against reference:" ++ (show err))


-- Process to execute BWA and write to <handle h> .sam file
mapToReference' newfp refIndex readSet = do
    bwaPath <- getBWAPath
    (_, Just hout, Just herr, jHandle) <-
        createProcess (
            proc 
                (bwaPath </> mapAlg)
                ["mem","-t",(show numCapabilities),(T.unpack refIndex), readSet]
            ) { std_out = CreatePipe,
                std_err = CreatePipe }
    writeToFile hout newfp
    hGetContents herr >>= printNglessLn
    return jHandle


convertSamToBam samfp newfp = do
    printNglessLn $ "Start to convert Sam to Bam. from " ++ samfp ++ " to -> " ++ newfp
    jHandle <- convSamToBam' samfp newfp
    exitCode <- waitForProcess jHandle
    case exitCode of
       ExitSuccess -> return (T.pack newfp)
       ExitFailure err -> error ("Failure on converting sam to bam" ++ (show err))

convSamToBam' samFP newfp = do
    samPath <- getSAMPath
    (_, Just hout, Just herr, jHandle) <- createProcess (
        proc 
            (samPath </> samAlg) 
            ["view", "-bS" ,samFP ]
        ){ std_out = CreatePipe,
           std_err = CreatePipe }
    writeToFile hout newfp 
    hGetContents herr >>= printNglessLn
    return jHandle


writeToFile :: Handle -> FilePath -> IO ()
writeToFile handle path = do
    contents <- B.hGetContents handle
    B.writeFile path contents
    hClose handle