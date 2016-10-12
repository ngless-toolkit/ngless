{-# LANGUAGE FlexibleContexts #-}
module NGLess.NGLEnvironment
    ( NGLEnvironment(..)
    , nglEnvironment
    , updateNglEnvironment
    ) where

import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import NGLess

data NGLEnvironment = NGLEnvironment
                    { ngleScriptText :: !T.Text
                    } deriving (Show, Eq)

ngle :: IORef NGLEnvironment
{-# NOINLINE ngle #-}
ngle = unsafePerformIO (newIORef $ NGLEnvironment "")

nglEnvironment :: NGLessIO NGLEnvironment
nglEnvironment = liftIO $ readIORef ngle

updateNglEnvironment :: (NGLEnvironment -> NGLEnvironment) -> NGLessIO ()
updateNglEnvironment = liftIO . modifyIORef' ngle
