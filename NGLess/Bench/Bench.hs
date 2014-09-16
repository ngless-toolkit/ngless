import Main
import Criterion.Main

main = defaultMain [ 
        bench "one" $ whnfIO . optsExec $ DefaultMode "ngless" "examples/annotation.ngl" 1
      ]
