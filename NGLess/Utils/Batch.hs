{- Copyright 2015-2020 NGLess Authors
 - License: MIT
 -}
module Utils.Batch
    ( getNcpus
    ) where

import Text.Read (readMaybe)
import System.Environment (lookupEnv)
import Control.Monad.Extra (firstJustM)

getNcpus :: IO (Maybe Int)
getNcpus = firstJustM getIntFromEnv
                 [ "OMP_NUM_THREADS"
                 , "NSLOTS"
                 , "LSB_DJOB_NUMPROC"
                 , "SLURM_CPUS_PER_TASK"
                 ]

getIntFromEnv :: String -> IO (Maybe Int)
getIntFromEnv evar = (>>= readMaybe) <$> lookupEnv evar
