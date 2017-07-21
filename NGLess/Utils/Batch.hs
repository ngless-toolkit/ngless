{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}
module Utils.Batch
    ( getNcpus
    ) where

import Text.Read (readMaybe)
import System.Environment (lookupEnv)


firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust [] = return Nothing
firstJust (x:xs) = x >>= \case
    Nothing -> firstJust xs
    val -> return val

getNcpus :: IO (Maybe Int)
getNcpus = firstJust . map getIntFromEnv $
                 [ "OMP_NUM_THREADS"
                 , "NSLOTS"
                 , "LSB_DJOB_NUMPROC"
                 , "SLURM_CPUS_PER_TASK"
                 ]

getIntFromEnv :: String -> IO (Maybe Int)
getIntFromEnv evar = (>>= readMaybe) <$> lookupEnv evar
