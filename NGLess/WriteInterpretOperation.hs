{-# LANGUAGE OverloadedStrings #-}

module WriteInterpretOperation
    (
    writeToFile
    ) where


import qualified Data.ByteString.Char8 as B

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad

import InvokeExternalProgs
import Language
import FileManagement

getNGOString (Just (NGOString s)) = s
getNGOString _ = error "Error: Type is different of String"

elFromMap el args = Map.lookup (T.pack el) (Map.fromList args)


writeToUncFile (NGOMappedReadSet path) newfp = do
    let path' = T.unpack path
    contents' <- readPossiblyCompressedFile (B.pack path') 
    write (T.unpack newfp) $ contents' 
    return $ NGOMappedReadSet newfp

writeToUncFile (NGOReadSet path enc tmplate) newfp = do
    let newfp' = T.unpack newfp
    contents' <- readPossiblyCompressedFile path
    write newfp' $ contents' 
    return $ NGOReadSet (B.pack newfp') enc tmplate

writeToUncFile err _ = error ("writeToUncFile: Should have received a NGOReadSet, but the type was: " ++ (show err))

writeToFile :: NGLessObject -> [(T.Text, NGLessObject)] -> IO NGLessObject   
writeToFile (NGOList el) args = do
    let templateFP = getNGOString ( elFromMap "ofile" args )
        indexFPs = map (T.pack . show) [1..(length el)]
        newFPS' = map (\x -> T.replace (T.pack "{index}") x templateFP) indexFPs
    res <- zipWithM (\x y -> writeToUncFile x y) el newFPS'
    return (NGOList res)

writeToFile el@(NGOReadSet _ _ _) args = do
    let newfp = getNGOString ( elFromMap "ofile" args )
    writeToUncFile el newfp

writeToFile el@(NGOMappedReadSet fp) args = do
    let map' = Map.fromList args
        newfp = getNGOString ( Map.lookup (T.pack "ofile") map') --
        format = Map.lookup (T.pack "format") map'
    case format of
        Nothing -> writeToUncFile el newfp
        Just x -> case x of 
            (NGOSymbol "bam") -> convertSamToBam (T.unpack fp) (T.unpack newfp) --newfp will contain the bam
            _     -> writeToUncFile el newfp 

writeToFile _ _ = error "Error: writeToFile Not implemented yet"
