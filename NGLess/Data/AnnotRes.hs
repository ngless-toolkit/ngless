{-# LANGUAGE OverloadedStrings #-}

module Data.AnnotRes
    ( GffCount(..)
      , readAnnotCounts
      , writeAnnotCount
      , showGffCountDel
      , showUniqIdCounts
      , filterByStrand
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.Map.Strict as Map

import Control.DeepSeq
import System.FilePath.Posix((</>), splitFileName)

import FileManagement

import Output
import Data.GFF

data GffCount = GffCount
            { annotSeqId :: S.ByteString
            , annotType :: GffType
            , annotCount :: !Int
            , annotStrand :: GffStrand
            } deriving (Eq,Show)

instance NFData GffCount where
    rnf (GffCount si t c s) = si `seq` t `seq` c `seq` s `seq` ()

    
showGffCount :: [GffCount] -> L8.ByteString 
showGffCount = L8.unlines . fmap (showCounts "\t")

showGffCountDel :: S8.ByteString -> [GffCount] -> L8.ByteString 
showGffCountDel del c = L8.unlines . fmap (showCounts del) $ c

showCounts :: S8.ByteString -> GffCount -> L8.ByteString
showCounts del (GffCount s t c st) = L8.fromChunks [s, del, encode t, del, encode c, del, showStrand st]
    where
        encode :: (Show a) => a -> B.ByteString
        encode = S8.pack . show --could be used Data.Binary (encode)


readAnnotCounts :: L.ByteString -> [GffCount]
readAnnotCounts = map readAnnotLine . L8.lines

readAnnotLine :: L.ByteString -> GffCount
readAnnotLine line = if length tokens == 4
            then GffCount
                (L8.toStrict tk0)
                (parsegffType $ L8.toStrict tk1)
                (read $ L8.unpack tk2)
                (strand $ L8.head tk3)
            else error (concat ["unexpected line in Annotated Gff: ", show line])
    where
        tokens = L8.split '\t' line
        [tk0,tk1,tk2,tk3] = tokens

writeAnnotCount :: FilePath -> [GffCount]-> IO FilePath
writeAnnotCount fn im = do
    temp <- getTemporaryDirectory 
    newfp <- getTempFilePath (temp </> (snd . splitFileName $ fn))
    outputLno' DebugOutput $ "Writing Annotation results to:" ++ newfp
    L8.writeFile newfp $ showGffCount im
    outputLno' InfoOutput "Write completed"
    return newfp

showUniqIdCounts :: S8.ByteString -> L8.ByteString -> L8.ByteString
showUniqIdCounts del cont = uniqueIdCountMap del . mergeIds . readAnnotCounts $ cont

uniqueIdCountMap :: S8.ByteString -> Map.Map S.ByteString Int -> L8.ByteString
uniqueIdCountMap del m = Map.foldrWithKey' (\k v r -> L8.append (showIdVal k v) r) L8.empty m
  where 
    encode = S8.pack . show
    showIdVal k v = L8.fromChunks [k, del, encode v, "\n"]

mergeIds :: [GffCount] -> Map.Map S.ByteString Int
mergeIds s = foldl updateM Map.empty s

updateM :: Map.Map S.ByteString Int -> GffCount -> Map.Map S.ByteString Int
updateM m g = Map.insertWith (+) (annotSeqId g) (annotCount g) m


filterByStrand :: GffStrand -> [GffCount] -> [GffCount]
filterByStrand s = filter filterByStrand'
  where 
    filterByStrand' g = isUnstrand (annotStrand g) || isSameStrand s (annotStrand g)
    isUnstrand    = (==GffUnStranded)
    isSameStrand  = (==)

