{- Copyright 2016
 - Licence: MIT -}
import Criterion.Main
import Interpretation.Map (_samStats)
import NGLess (testNGLessIO)

nfNGLessIO = nfIO . testNGLessIO

main = defaultMain [
    bgroup "sam-stats"
        [ bench "sample" $ nfNGLessIO (_samStats "test_samples/sample.sam")
        ]
    ]
