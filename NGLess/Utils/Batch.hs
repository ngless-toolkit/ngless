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
getNcpus = firstJust
            [ getOpenMPThreads
            , getGEncpus 
            , getLSFncpus
            ]

getLSFncpus :: IO (Maybe Int)
getLSFncpus = (>>= readMaybe) <$> lookupEnv "LSB_DJOB_NUMPROC"

getGEncpus :: IO (Maybe Int)
getGEncpus = (>>= readMaybe) <$> lookupEnv "NSLOTS"

getOpenMPThreads :: IO (Maybe Int)
getOpenMPThreads = (>>= readMaybe) <$> lookupEnv "OMP_NUM_THREADS"
