{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Interpretation.Count
    ( executeCount
    , MMMethod(..)
    , _performCount
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.IO (hClose)
import Data.Convertible
import Data.Function (on)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($$), ($=), (=$=))

import Language

import NGLess
import Output
import FileManagement
import Data.Annotation
import Data.Maybe
import Data.GFF
import Utils.Vector

data MMMethod = MMCountAll | MM1OverN | MMDist1
    deriving (Eq, Show)


methodFor "1overN" = return MM1OverN
methodFor "dist1" = return MMDist1
methodFor "all1" = return MMCountAll
methodFor other = throwShouldNotOccur (T.concat ["Unexpected multiple method ", other])

executeCount :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCount (NGOList e) args = NGOList <$> mapM (`executeCount` args) e
executeCount (NGOAnnotatedSet gname annot_fp headers_fp) args = do
    let c = lookup "counts" args
        c' = GffGene
    minCount <- lookupIntegerOrScriptErrorDef (return 0) "count argument parsing" "min" args
    method <- methodFor =<< lookupSymbolOrScriptErrorDef (return "dist1")
                                    "multiple argument to count " "multiple" args
    NGOCounts <$> _performCount headers_fp annot_fp gname minCount method
executeCount err _ = throwScriptError ("Invalid Type. Should be used NGOList or NGOAnnotatedSet but type was: " ++ show err)


_performCount :: FilePath -> FilePath -> T.Text -> Integer -> MMMethod -> NGLessIO FilePath
_performCount headers_fp annot_fp gname minCount method = do
    let extractSecond hline = case B8.split '\t' hline of
            [_, scol] -> return scol
            _ -> throwDataError ("Could not parse internal intermediate file '"++headers_fp++"'. This may be a bug in ngless or your system is not preserving temp files")

        -- like Python's enumerate() as a Conduit
        enumerate :: (Monad m) => C.Conduit a m (Int,a)
        enumerate = loop 0
            where
                loop !n = C.await >>= \case
                            Nothing -> return ()
                            Just v -> do
                                C.yield (n,v)
                                loop (n+1)
    index <- CB.sourceFile headers_fp
                $= CB.lines
                =$= CL.mapM extractSecond
                =$= enumerate
                $$ CL.fold (\m (i,a) -> M.insert a i m) M.empty

    just_idxs <- CB.sourceFile annot_fp
        $= CB.lines
        =$= CL.mapM (\line -> case decodeAR line of
                            Right v -> return v
                            Left err -> throwDataError err)
        =$= CL.groupBy ((==) `on` readId)
        =$= CL.mapM (asIndices index)
        $$ CL.consume

    let n_headers = length (M.elems index)
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
        BL.hPut hout (BL.fromChunks ["\t", T.encodeUtf8 gname, "\n"])
        forM_ (zip (M.keys index) [0..]) $ \(hn,i) -> do
            v <- V.indexM result i
            when (v > fromIntegral minCount) $
                BL.hPut hout (BL.fromChunks [hn, "\t", B8.pack . show $ v, "\n"])
        hClose hout
    return newfp

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
                            then (/ cs_sum)
                            else const  (1.0 / n_cs)
            forM_ (zip vs cs) $ \(v,c) ->
                unsafeIncrement' ncounts v (adjust c)
    when fractionResult $
        toFractions ncounts
    return ncounts


asIndices :: M.Map B.ByteString Int -> [AnnotatedRead] -> NGLessIO [Int]
asIndices index = mapM lookupIx
    where
        lookupIx :: AnnotatedRead -> NGLessIO Int
        lookupIx val = case M.lookup (annotValue val) index of
            Just v -> return v
            Nothing -> throwDataError ("Count: annotation not in header!" :: String)
