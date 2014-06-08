{-# LANGUAGE OverloadedStrings #-}

module Data.AnnotRes
    ( 
      GffCount(..)
      , showGffCount
      , readAnnotCounts
      , filterCounts
      , isMinAmount
      , writeAnnotCount
      , showGffCountDel
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.DeepSeq
import System.FilePath.Posix((</>), splitFileName)

import FileManagement
import Language

import Data.GFF

data GffCount = GffCount
            { annotSeqId :: S.ByteString
            , annotType :: GffType
            , annotCount :: Int
            } deriving (Eq,Show)

instance NFData GffCount where
    rnf gl =(annotSeqId gl) `seq`
            (annotType gl) `seq`
            (annotCount gl) `seq`
            ()

showGffCount :: [GffCount] -> L8.ByteString 
showGffCount = L8.unlines . fmap (showCounts "\t")

showGffCountDel :: S8.ByteString -> [GffCount] -> L8.ByteString 
showGffCountDel del c = L8.unlines . fmap (showCounts del) $ c

showCounts :: S8.ByteString -> GffCount -> L8.ByteString
showCounts del (GffCount s t c) = L8.fromChunks [s, del, showType t, del, encode c]
    where encode = S8.pack . show --could be used Data.Binary (encode)


readAnnotCounts :: L.ByteString -> [GffCount]
readAnnotCounts = readAnnotCounts' . L8.lines

readAnnotCounts' :: [L.ByteString] -> [GffCount]
readAnnotCounts' [] = []
readAnnotCounts' (l:ls) = readAnnotLine l : readAnnotCounts' ls

readAnnotLine :: L.ByteString -> GffCount
readAnnotLine line = if length tokens == 3
            then GffCount
                (L8.toStrict tk0)
                (parsegffType $ L8.toStrict tk1)
                (read $ L8.unpack tk2)
            else error (concat ["unexpected line in Annotated Gff: ", show line])
    where
        tokens = L8.split '\t' line
        [tk0,tk1,tk2] = tokens


--- Quite similar to the filterFeatures in GFF but access different field name.
filterCounts :: Maybe NGLessObject -> GffCount -> Bool
filterCounts feats annotL = maybe True (fFeat annotL) feats
  where 
        fFeat g (NGOList f) = foldl (\a b -> a || b) False (map (filterCounts' g) f)
        fFeat _ err = error("Type should be NGOList but received: " ++ (show err))

filterCounts' :: GffCount -> NGLessObject -> Bool
filterCounts' g (NGOSymbol "gene") = (==GffGene) . annotType $ g
filterCounts' g (NGOSymbol "exon") = (==GffExon) . annotType $ g
filterCounts' g (NGOSymbol "cds" ) = (==GffCDS) . annotType  $ g
filterCounts' g (NGOSymbol s) = (S8.unpack . showType . annotType $ g) == (T.unpack s)
filterCounts' _ err = error ("Type should be NGOList but received: " ++ (show err))


isMinAmount :: NGLessObject -> GffCount ->  Bool
isMinAmount (NGOInteger l) g = (toInteger $ annotCount g) >= l
isMinAmount err _ = error ("Type should be NGOInteger but received: " ++ (show err))

writeAnnotCount :: FilePath -> [GffCount] -> IO T.Text
writeAnnotCount fn im = do
    temp <- getTemporaryDirectory 
    newfp <- getTFilePathComp (temp </> (snd . splitFileName $ fn))
    printNglessLn $ "Writing Annotation results to:" ++ newfp
    writeGZIP newfp $ showGffCount im
    return .  T.pack $ newfp
