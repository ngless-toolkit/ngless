{- Copyright 2016
 - Licence: MIT -}
import Criterion.Main



import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit as C
import           Data.Conduit ((=$=), ($$))
import           Control.DeepSeq (NFData)

import Control.Monad.Trans.Resource (runResourceT)

import NGLess (NGLessIO, testNGLessIO)
import Configuration (setupTestConfiguration)


import Interpretation.Map (_samStats)
import Interpretation.Count (performCount, MMMethod(..), loadAnnotator, loadFunctionalMap, CountOpts(..), annotationRule, AnnotationIntersectionMode(..), AnnotationMode(..))
import Data.Sam (readSamLine, readSamGroupsC)
import Data.GFF

nfNGLessIO :: (NFData a) => NGLessIO a -> Benchmarkable
nfNGLessIO = nfIO . testNGLessIO


nfRIO = nfIO . runResourceT
countRights = loop (0 :: Int)
    where
        loop !i = C.await >>= \case
            Nothing -> return i
            Just (Right _) -> loop (i+1)
            _ -> loop i

count= loop (0 :: Int)
    where
        loop !i = C.await >>= \case
            Nothing -> return i
            Just _ -> loop (i+1)

basicCountOpts = CountOpts
        { optFeatures = []
        , optIntersectMode = annotationRule IntersectStrict
        , optStrandSpecific = False
        , optKeepAmbiguous = True
        , optMinCount = 0.0
        , optMMMethod = MMDist1
        , optDelim = "\t"
        }

main = setupTestConfiguration >> defaultMain [
    bgroup "sam-stats"
        [ bench "sample" $ nfNGLessIO (_samStats "test_samples/sample.sam")
        ]
    ,bgroup "parse-sam"
        [ bench "readSamLine" $ nfRIO (CB.sourceFile "test_samples/sample.sam" =$= CB.lines =$= CL.map readSamLine $$ countRights)
        , bench "samGroups" $ nfNGLessIO (CB.sourceFile "test_samples/sample.sam" =$= CB.lines =$= readSamGroupsC $$ count)
        ]
    ,bgroup "count"
        [ bench "load-map"      $ nfNGLessIO (loadFunctionalMap   "test_samples/functional.map" ["ko", "cog"])
        , bench "annotate-seqname" . nfNGLessIO $ do
                    amap <- loadAnnotator (AnnotateFunctionalMap "test_samples/functional.map") basicCountOpts
                    performCount "test_samples/sample.sam" "testing" amap basicCountOpts
        ]
    ]
