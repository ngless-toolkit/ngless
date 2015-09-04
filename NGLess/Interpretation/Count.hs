module Interpretation.Count
    ( executeCount
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashTable.IO as HIO
import qualified Data.HashTable.ST.Basic as HB
import System.IO (hClose)
import Data.Either
import Data.Maybe
import Data.List (sort, groupBy)
import Data.Function (on)

import Language

import NGLess
import FileManagement
import Data.Annotation
import Data.GFF
import Utils.Debug
import Utils.Utils

uniq [] = []
uniq [a] = [a]
uniq (a:b:rs)
    | a /= b = a:uniq (b:rs)
    | otherwise = uniq (b:rs)

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOAnnotatedSet annot_fp headers_fp) args = do
    let c = lookup "counts" args
        NGOInteger m = lookupWithDefault (NGOInteger 0) "min" args
        c' = GffGene
    annotated' <- map decodeAR . BL8.lines <$> liftIO (BL.readFile annot_fp)
    (newfp,hout) <- openNGLTempFile annot_fp "counts." "txt"

    let (errors, annotated) = partitionEithers annotated'
        grouped = groupBy ((==) `on` annotValue) . filter ((==c') . annotType) $ annotated
    liftIO $ do
        h <- HIO.new :: IO (HIO.BasicHashTable B.ByteString Double)
        forM_ grouped $ \vs -> do
            let n = 1 -- length vs
            forM_ vs $ \v -> do
                let v' = annotValue v
                cur <- fromMaybe (0.0  :: Double) <$> HIO.lookup h v'
                HIO.insert h v' $! ( ((1.0/fromIntegral n)+cur) :: Double)
        let second_col hline = (BL8.split '\t' hline) !! 1
        ids <- map (BL8.toStrict . second_col ) . BL8.lines <$> BL.readFile headers_fp
        forM_ (uniq $ sort ids) $ \hn -> do
            v <- fromMaybe 0.0 <$> HIO.lookup h hn
            when (v > fromIntegral m) $
                BL.hPut hout (BL.fromChunks [hn, "\t", B8.pack . show . round $ v, "\n"])
        hClose hout
    forM_ errors throwDataError
    return $ NGOCounts newfp
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


