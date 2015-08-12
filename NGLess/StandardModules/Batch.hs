{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Batch
    ( loadModule
    ) where

import System.Environment (lookupEnv)
import Control.Concurrent (setNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Text.Read
import Data.Maybe

import Language
import Modules
import NGLess

getLSFncpus :: IO (Maybe Int)
getLSFncpus = (>>= readMaybe) <$> lookupEnv "LSB_DJOB_NUMPROC"

getLSFjobIndex :: IO (Maybe Integer)
getLSFjobIndex = (>>= readMaybe) <$> lookupEnv "LSB_JOBINDEX"

getGEncpus :: IO (Maybe Int)
getGEncpus = (>>= readMaybe) <$> lookupEnv "NSLOTS"

getGEjobIndex :: IO (Maybe Integer)
getGEjobIndex = (>>= readMaybe) <$> lookupEnv "SGE_TASK_ID"


getJobIndex :: IO (Maybe Integer)
getJobIndex = liftA2 (<|>) getLSFjobIndex getGEjobIndex

getNcpus :: IO (Maybe Int)
getNcpus = liftA2 (<|>) getLSFncpus getGEncpus

loadModule :: T.Text -> NGLessIO Module
loadModule _ = liftIO $ do
    job_id <- getJobIndex
    ncpus <- getNcpus
    when (isJust ncpus) $
        setNumCapabilities (fromJust ncpus)
    return Module
        { modInfo = ModInfo "stdlib.batch" "0.0"
        , modConstants =
                [("JOBINDEX_OR_0", NGOInteger (fromMaybe 0 job_id))
                ,("JOBINDEX_VALID", NGOBool (isJust job_id))
                ]
        , modFunctions = []
        , runFunction = \_ _ _ -> return NGOVoid
        , validateFunction = const (return [])
        }
