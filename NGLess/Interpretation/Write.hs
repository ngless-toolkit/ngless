{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Interpretation.Write
    ( writeToFile
    ) where


import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Map as M
import System.Directory (canonicalizePath)
import System.Process
import System.Exit
import System.IO
import Data.String.Utils
import Data.Maybe

import Language
import FileManagement
import JSONManager
import Configuration
import Output
import Data.AnnotRes

getNGOPath (Just (NGOFilename p)) = p
getNGOPath (Just (NGOString p)) = T.unpack p
getNGOPath _ = error "getNGOPath cannot decode file path"

writeToUncFile :: NGLessObject -> FilePath -> IO NGLessObject
writeToUncFile (NGOMappedReadSet path defGen) newfp = do
    readPossiblyCompressedFile path >>= BL.writeFile newfp
    return $ NGOMappedReadSet newfp defGen

writeToUncFile (NGOReadSet path enc tmplate) newfp = do
    readPossiblyCompressedFile path >>= BL.writeFile newfp
    return $ NGOReadSet newfp enc tmplate

writeToUncFile obj _ = error ("writeToUncFile: Should have received a NGOReadSet or a NGOMappedReadSet but the type was: " ++ show obj)


writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject
writeToFile (NGOList el) args = do
      let templateFP = getNGOPath $ lookup "ofile" args
          newFPS' = map (T.pack . (\fname -> replace "{index}" fname templateFP) . T.unpack) indexFPs
      res <- zipWithM (\x fp -> writeToFile x (fp' fp)) el newFPS'
      return (NGOList res)
    where
        indexFPs = map (T.pack . show) [1..(length el)]
        fp' fp = M.toList $ M.insert "ofile" (NGOString fp) (M.fromList args)

writeToFile el@(NGOReadSet _ _ _) args = writeToUncFile el $ getNGOPath (lookup "ofile" args)
writeToFile el@(NGOMappedReadSet fp defGen) args = do
    let newfp = getNGOPath (lookup "ofile" args) --
        format = fromMaybe (NGOSymbol "sam") (lookup "format" args)
    case format of
        NGOSymbol "sam" -> writeToUncFile el newfp
        NGOSymbol "bam" -> do
                        newfp' <- convertSamToBam fp newfp
                        return (NGOMappedReadSet newfp' defGen) --newfp will contain the bam
        _ -> error "This format should have been impossible"

writeToFile (NGOAnnotatedSet fp) args = do
    let newfp = getNGOPath $ lookup "ofile" args
        del = getDelimiter $ lookup "format" args
    output InfoOutput $ "Writing your NGOAnnotatedSet to: " ++ newfp
    cont <- readPossiblyCompressedFile fp
    case lookup "verbose" args of
        Just (NGOSymbol "no")  -> writeAnnotResWDel' newfp $ showUniqIdCounts del cont
        Just (NGOSymbol "yes") -> writeAnnotResWDel' newfp (showGffCountDel del . readAnnotCounts $ cont)
        Just err -> error ("verbose received a " ++ show err ++ " but value can only be yes or no.")
        Nothing -> writeAnnotResWDel' newfp $ showUniqIdCounts del cont
    where
        writeAnnotResWDel' p cont = do
            BL.writeFile p cont
            canonicalizePath p >>= insertCountsProcessedJson
            return $ NGOAnnotatedSet p
writeToFile _ _ = error "Error: writeToFile Not implemented yet"

getDelimiter :: Maybe NGLessObject -> B.ByteString
getDelimiter (Just (NGOSymbol "csv")) = ","
getDelimiter (Just (NGOSymbol "tsv")) = "\t"
getDelimiter Nothing = "\t"
getDelimiter (Just v) =  error ("Type of 'format' in 'write' must be NGOSymbol, got " ++ show v)

convertSamToBam samfile newfp = do
    outputList DebugOutput ["SAM->BAM Conversion start ('", samfile, "' -> '", newfp, "')"]
    samPath <- samtoolsBin
    withFile newfp WriteMode $ \hout -> do
        (_, _, Just herr, jHandle) <- createProcess (
            proc samPath
                ["view", "-bS", samfile]
            ){ std_out = UseHandle hout,
               std_err = CreatePipe }
        hGetContents herr >>= putStrLn
        exitCode <- waitForProcess jHandle
        case exitCode of
           ExitSuccess -> return newfp
           ExitFailure err -> error ("Failure on converting sam to bam" ++ show err)
