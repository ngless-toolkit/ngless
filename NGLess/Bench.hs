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
import Interpretation.Annotation (_annotateSeqname, annotateMap, AnnotationOpts(..), _intersection_strict, loadFunctionalMap)
import Interpretation.Count (_performCount, MMMethod(..))
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

main = setupTestConfiguration >> defaultMain [
    bgroup "sam-stats"
        [ bench "sample" $ nfNGLessIO (_samStats "test_samples/sample.sam")
        ]
    ,bgroup "annotation"
        [ bench "annotate-seqname" $ nfNGLessIO (_annotateSeqname "test_samples/sample.sam" undefined)
        , bench "annotate-map"     $ nfNGLessIO (annotateMap     "test_samples/sample.sam" "test_samples/functional.map"
                                        (AnnotationOpts [GffOther "ko", GffOther "cog"] _intersection_strict False True))
        , bench "load-map"     $ nfNGLessIO (loadFunctionalMap   "test_samples/functional.map" ["ko", "cog"])
        ]
    ,bgroup "parse-sam"
        [ bench "readSamLine" $ nfRIO (CB.sourceFile "test_samples/sample.sam" =$= CB.lines =$= CL.map readSamLine $$ countRights)
        , bench "samGroups" $ nfNGLessIO (CB.sourceFile "test_samples/sample.sam" =$= CB.lines =$= readSamGroupsC $$ count)
        ]
    ,bgroup "count"
        [ bench "count-base" $ nfNGLessIO (_performCount "test_samples/annotation_headers.txt" "test_samples/annotated.tsv" "benching" 0 MMDist1)
        ]
    ]
