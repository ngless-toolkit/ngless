{- Copyright 2016
 - Licence: MIT -}
import Criterion.Main
import Interpretation.Map (_samStats)
import Interpretation.Annotation (_annotateSeqname)
import Control.DeepSeq (NFData)

import NGLess (NGLessIO, testNGLessIO)
import Configuration (setupTestConfiguration)


nfNGLessIO :: (NFData a) => NGLessIO a -> Benchmarkable
nfNGLessIO = nfIO . testNGLessIO

main = setupTestConfiguration >> defaultMain [
    bgroup "sam-stats"
        [ bench "sample" $ nfNGLessIO (_samStats "test_samples/sample.sam")
        ]
    ,bgroup "annotation"
        [ bench "annotate-seqname" $ nfNGLessIO (_annotateSeqname "test_samples/sample.sam" undefined)
        ]
    ]
