{-# LANGUAGE OverloadedStrings #-}

module WriteInterpretOperation
    (
    writeToFile
    ) where


import qualified Data.ByteString.Char8 as B

import qualified Data.Text as T

import Control.Monad

import InvokeExternalProgs
import Language
import FileManagement

import Data.AnnotRes

getNGOString (Just (NGOString s)) = s
getNGOString _ = error "Error: Type is different of String"


writeToUncFile (NGOMappedReadSet path defGen) newfp = do
    let path' = T.unpack path
    contents' <- readPossiblyCompressedFile (B.pack path') 
    write (T.unpack newfp) $ contents' 
    return $ NGOMappedReadSet newfp defGen

writeToUncFile (NGOReadSet path enc tmplate) newfp = do
    let newfp' = T.unpack newfp
    contents' <- readPossiblyCompressedFile path
    write newfp' $ contents' 
    return $ NGOReadSet (B.pack newfp') enc tmplate

writeToUncFile err _ = error ("writeToUncFile: Should have received a NGOReadSet or a NGOMappedReadSet but the type was: " ++ (show err))

writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject   
writeToFile (NGOList el) args = do
    let templateFP = getNGOString ( lookup "ofile" args )
        indexFPs = map (T.pack . show) [1..(length el)]
        newFPS' = map (\x -> T.replace (T.pack "{index}") x templateFP) indexFPs
    res <- zipWithM (\x y -> writeToUncFile x y) el newFPS'
    return (NGOList res)

writeToFile el@(NGOReadSet _ _ _) args = do
    let newfp = getNGOString ( lookup "ofile" args )
    writeToUncFile el newfp

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
    let newfp = getNGOString (lookup "ofile" args)
    printNglessLn $ "Writing your NGOAnnotatedSet to: " ++ (T.unpack newfp)
    cont <- unCompress (T.unpack fp)
    writeAnnotResWDel' newfp cont $ getDelimiter (lookup "format" args)
    where
        writeAnnotResWDel' p cont del = do
            write (T.unpack p) (showGffCountDel del . readAnnotCounts $ cont)
            return $ NGOAnnotatedSet p
            

writeToFile _ _ = error "Error: writeToFile Not implemented yet"


getDelimiter :: Maybe NGLessObject -> B.ByteString
getDelimiter x = case x of
        (Just (NGOSymbol "csv")) -> ","
        (Just (NGOSymbol "tsv")) -> "\t"
        (Just err) ->  error ("Type must be NGOSymbol, but was given" ++ (show err))
        Nothing -> "\t"