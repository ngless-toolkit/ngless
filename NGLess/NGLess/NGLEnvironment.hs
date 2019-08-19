{- Copyright 2016-2019 NGLess Authors
 - License: MIT
 -}
module NGLess.NGLEnvironment
    ( NGLVersion(..)
    , NGLEnvironment(..)
    , nglEnvironment
    , nglConfiguration
    , updateNglEnvironment
    , updateNglEnvironment'
    , setQuiet
    , setupTestEnvironment
    ) where

import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import NGLess.NGError
import Configuration
import CmdArgs (Verbosity(..))

data NGLVersion = NGLVersion !Int !Int
                        deriving (Eq, Show)

instance Ord NGLVersion where
    compare (NGLVersion majV0 minV0) (NGLVersion majV1 minV1)
        | majV0 == majV1 = compare minV0 minV1
        | otherwise = compare majV0 majV1

data NGLEnvironment = NGLEnvironment
                    { ngleVersion :: !NGLVersion
                    , ngleLno :: !(Maybe Int)
                    , ngleScriptText :: !T.Text -- ^ The original text of the script
                    , ngleMappersActive :: [T.Text] -- ^ which mappers can be used
                    , ngleTemporaryFilesCreated :: [FilePath] -- ^ list of temporary files created
                    , ngleConfiguration :: NGLessConfiguration
                    } deriving (Show, Eq)

ngle :: IORef NGLEnvironment
{-# NOINLINE ngle #-}
ngle = unsafePerformIO (newIORef $ NGLEnvironment (NGLVersion 0 0) Nothing "" ["bwa"] [] (error "Configuration not set"))

nglEnvironment :: NGLessIO NGLEnvironment
nglEnvironment = liftIO $ readIORef ngle

updateNglEnvironment' :: (NGLEnvironment -> NGLEnvironment) -> IO ()
updateNglEnvironment' = modifyIORef' ngle

updateNglEnvironment :: (NGLEnvironment -> NGLEnvironment) -> NGLessIO ()
updateNglEnvironment = liftIO . updateNglEnvironment'

nglConfiguration :: NGLessIO NGLessConfiguration
nglConfiguration = ngleConfiguration <$> nglEnvironment

-- | sets verbosity to Quiet
setQuiet :: NGLessIO ()
setQuiet = updateNglEnvironment (\e -> e { ngleConfiguration = setQuiet' (ngleConfiguration e) })
    where
        setQuiet' c = c { nConfVerbosity = Quiet, nConfPrintHeader = False }

-- | setup an environment that can be used for testing
setupTestEnvironment :: IO ()
setupTestEnvironment = do
    config <- guessConfiguration
    let config' = config { nConfTemporaryDirectory = "testing_tmp_dir", nConfKeepTemporaryFiles = True, nConfVerbosity = Quiet }
    updateNglEnvironment' (\env -> env { ngleConfiguration = config' })
