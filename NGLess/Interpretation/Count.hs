{- Copyright 2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase #-}
module Interpretation.Count
    ( executeCount
    ) where

import Control.Monad
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.IO (hClose)
import Data.Either
import Data.Convertible
import Data.List (groupBy)
import Data.Function (on)

import Language

import NGLess
import Output
import FileManagement
import Data.Annotation
import Data.Maybe
import Data.GFF
import Utils.Vector
import Utils.Utils

data MMMethod = MMCountAll | MM1OverN | MMDist1
    deriving (Eq, Show)


methodFor "1overN" = return MM1OverN
methodFor "dist1" = return MMDist1
methodFor "all1" = return MMCountAll
methodFor other = throwShouldNotOccur (T.concat ["Unexpected multiple method ", other])

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOAnnotatedSet annot_fp headers_fp) args = do
    let c = lookup "counts" args
        NGOInteger m = lookupWithDefault (NGOInteger 0) "min" args
        c' = GffGene
    methodS <- symbolOrTypeError "multiple argument to count " . lookupWithDefault (NGOSymbol "dist1") "multiple" $ args
    method <- methodFor methodS
    let second_col hline = BL8.split '\t' hline !! 1
    ids <- map (BL8.toStrict . second_col) . BL8.lines <$> liftIO (BL.readFile headers_fp)
    let index = M.fromList (zip ids [0..])
        n_headers = length (M.elems index)

    annotated' <- map decodeAR . BL8.lines <$> liftIO (BL.readFile annot_fp)
    let (errors, annotated) = partitionEithers annotated'
    forM_ errors throwDataError
    just_idxs <- asIndices index annotated

    result <- case method of
        MMCountAll -> do
            outputListLno' TraceOutput ["Counts (all 1 method)..."]
            return $ all1 just_idxs n_headers

        MMDist1 -> do
            outputListLno' TraceOutput ["Counts (first pass)..."]
            let firstpass = distributeMM just_idxs n_headers Nothing True

            outputListLno' TraceOutput ["Counts (second pass)..."]
            let secondpass = distributeMM just_idxs n_headers (Just firstpass) False
            return secondpass
        MM1OverN -> do
            outputListLno' TraceOutput ["Counts (1 over n method)..."]
            return $ oneOverN just_idxs n_headers

    (newfp,hout) <- openNGLTempFile annot_fp "counts." "txt"
    liftIO $ do
        forM_ (zip (M.keys index) [0..]) $ \(hn,i) -> do
            v <- V.indexM result i
            when (v > fromIntegral m) $
                BL.hPut hout (BL.fromChunks [hn, "\t", B8.pack . show $ v, "\n"])
        hClose hout
    return $ NGOCounts newfp
executeCount err _ = error ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


all1 = all1OrOneOverN True
oneOverN = all1OrOneOverN False
all1OrOneOverN isAll1 indices n = V.create $ do
    counts <- VM.replicate n (0.0 :: Double)
    forM_ indices $ \cs -> do
        let nc = length cs
        forM_ cs $ \i -> do
            let inc = if isAll1 then 1.0 else (1.0 / convert nc)
            unsafeIncrement' counts i inc
    return counts

distributeMM indices n current fractionResult = V.create $ do
    let Just current' = current
    ncounts <- VM.replicate n (0.0 :: Double)
    forM_ indices $ \case
        [v] -> unsafeIncrement' ncounts v 1.0
        vs -> when (isJust current) $ do
            let cs = map (V.unsafeIndex current') vs
                cs_sum = sum cs
                n_cs = convert (length cs)
                adjust :: Double -> Double
                adjust = if cs_sum > 0.0
                            then (\c -> c / cs_sum)
                            else const  (1.0 / n_cs)
            forM_ (zip vs cs) $ \(v,c) -> do
                unsafeIncrement' ncounts v (adjust c)
    when fractionResult $
        toFractions ncounts
    return ncounts


asIndices :: M.Map B.ByteString Int -> [AnnotatedRead] -> NGLessIO [[Int]]
asIndices index annotated = mapM (mapM lookupIx) $ groupBy ((==) `on` readId) annotated
    where
        lookupIx :: AnnotatedRead -> NGLessIO Int
        lookupIx val = case M.lookup (annotValue val) index of
            Just v -> return v
            Nothing -> throwDataError ("Cannot header value lookup in count" :: String)
