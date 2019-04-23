{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

module StandardModules.Batch
    ( loadModule
    ) where

import System.Environment (lookupEnv)
import Control.Concurrent (setNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Data.Maybe
import Data.Default

import Language
import Modules
import NGLess
import Utils.Batch (getNcpus)

getLSFjobIndex :: IO (Maybe Integer)
getLSFjobIndex = (>>= readMaybe) <$> lookupEnv "LSB_JOBINDEX"

getGEjobIndex :: IO (Maybe Integer)
getGEjobIndex = (>>= readMaybe) <$> lookupEnv "SGE_TASK_ID"


getJobIndex :: IO (Maybe Integer)
getJobIndex = liftA2 (<|>) getLSFjobIndex getGEjobIndex

loadModule :: T.Text -> NGLessIO Module
loadModule _ = liftIO $ do
    job_id <- getJobIndex
    ncpus <- getNcpus
    when (isJust ncpus) $
        setNumCapabilities (fromJust ncpus)
    return def
        { modInfo = ModInfo "stdlib.batch" "1.0"
        , modConstants =
                [("JOBINDEX_OR_0", NGOInteger (fromMaybe 0 job_id))
                ,("JOBINDEX_VALID", NGOBool (isJust job_id))
                ]
        }
