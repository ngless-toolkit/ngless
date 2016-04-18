{- Copyright 2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
module Hooks
    ( Hook(..)
    , registerHook
    , triggerHook
    ) where


import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import NGLess

data Hook = FinishOkHook
    deriving (Eq, Show, Ord, Bounded, Enum)

nglHooks :: IORef [(Hook, NGLessIO ())]
{-# NOINLINE nglHooks #-}
nglHooks = unsafePerformIO (newIORef [])

registerHook :: Hook -> NGLessIO () -> NGLessIO ()
registerHook hook act = liftIO $ modifyIORef nglHooks ((hook,act):)

triggerHook :: Hook -> NGLessIO ()
triggerHook hook = do
    registered <- liftIO $ readIORef nglHooks
    forM_ registered $ \(h,act) ->
        when (h == hook) act
