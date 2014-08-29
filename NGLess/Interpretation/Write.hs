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

import Language
import FileManagement
import JSONManager
import Configuration
import Data.AnnotRes

getNGOString (Just (NGOString s)) = s
getNGOString _ = error "Error: Type is different of String"

writeToUncFile (NGOMappedReadSet path defGen) newfp = do
    let path' = B.pack . T.unpack $ path
    readPossiblyCompressedFile path' >>= BL.writeFile (T.unpack newfp)
    return $ NGOMappedReadSet newfp defGen

writeToUncFile (NGOReadSet path enc tmplate) newfp = do
    let newfp' = T.unpack newfp
    readPossiblyCompressedFile path >>= BL.writeFile newfp'
    return $ NGOReadSet (B.pack newfp') enc tmplate

writeToUncFile err _ = error ("writeToUncFile: Should have received a NGOReadSet or a NGOMappedReadSet but the type was: " ++ (show err))


writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject   
writeToFile (NGOList el) args = do
      let templateFP = getNGOString $ lookup "ofile" args
          newFPS' = map (\x -> T.replace "{index}" x templateFP) indexFPs
      res <- zipWithM (\x fp -> writeToFile x (fp' fp)) el newFPS'
      return (NGOList res)
    where
        indexFPs = map (T.pack . show) [1..(length el)]
        fp' fp = M.toList $ M.insert "ofile" (NGOString fp) (M.fromList args)

writeToFile el@(NGOReadSet _ _ _) args = writeToUncFile el $ getNGOString ( lookup "ofile" args )

writeToFile el@(NGOMappedReadSet fp defGen) args = do
    let newfp = getNGOString (lookup (T.pack "ofile") args) --
        format = lookup "format" args
    case format of
        Nothing -> writeToUncFile el newfp
        Just format' -> case format' of 
            (NGOSymbol "bam") -> convertSamToBam (T.unpack fp) (T.unpack newfp) 
                                            >>= \x -> return $ NGOMappedReadSet x defGen --newfp will contain the bam
            _     -> writeToUncFile el newfp -- Sam file is the kept format so no need to convert.

writeToFile (NGOAnnotatedSet fp) args = do
    let newfp = getNGOString $ lookup "ofile" args
        del = getDelimiter  $ lookup "format" args
    printNglessLn $ "Writing your NGOAnnotatedSet to: " ++ (T.unpack newfp)
    cont <- unCompress (T.unpack fp)
    case lookup "verbose" args of
        Just (NGOSymbol "no")  -> writeAnnotResWDel' newfp $ showUniqIdCounts del cont
        Just (NGOSymbol "yes") -> writeAnnotResWDel' newfp (showGffCountDel del . readAnnotCounts $ cont)
        Just err -> error ("verbose received a " ++ (show err) ++ " but value can only be yes or no.")
        Nothing -> writeAnnotResWDel' newfp $ showUniqIdCounts del cont
    where
        writeAnnotResWDel' p cont = do
            BL.writeFile (T.unpack p) cont
            canonicalizePath (T.unpack p) >>= insertCountsProcessedJson 
            return $ NGOAnnotatedSet p
            
writeToFile _ _ = error "Error: writeToFile Not implemented yet"



getDelimiter :: Maybe NGLessObject -> B.ByteString
getDelimiter x = case x of
        (Just (NGOSymbol "csv")) -> ","
        (Just (NGOSymbol "tsv")) -> "\t"
        (Just err) ->  error ("Type must be NGOSymbol, but was given" ++ (show err))
        Nothing -> "\t"

convertSamToBam samfp newfp = do
    printNglessLn $ "Start to convert Sam to Bam. from " ++ samfp ++ " to -> " ++ newfp
    jHandle <- convSamToBam' samfp newfp
    exitCode <- waitForProcess jHandle
    case exitCode of
       ExitSuccess -> return (T.pack newfp)
       ExitFailure err -> error ("Failure on converting sam to bam" ++ (show err))

convSamToBam' samFP newfp = do
    samPath <- samtoolsBin
    (_, Just hout, Just herr, jHandle) <- createProcess (
        proc samPath
            ["view", "-bS" ,samFP ]
        ){ std_out = CreatePipe,
           std_err = CreatePipe }
    writeToFile' hout newfp
    hGetContents herr >>= printNglessLn
    return jHandle

writeToFile' :: Handle -> FilePath -> IO ()
writeToFile' handle path = do
    contents <- B.hGetContents handle
    B.writeFile path contents
    hClose handle

