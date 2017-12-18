{- Copyright 2016-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts #-}
module Hooks
    ( Hook(..)
    , registerHook
    , registerFailHook
    , triggerFailHook
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

failHooks :: IORef [IO ()]
{-# NOINLINE failHooks #-}
failHooks = unsafePerformIO (newIORef [])

registerHook :: Hook -> NGLessIO () -> NGLessIO ()
registerHook hook act = liftIO $ modifyIORef nglHooks ((hook,act):)

registerFailHook :: IO () -> NGLessIO ()
registerFailHook act = liftIO $ modifyIORef failHooks (act:)

triggerFailHook :: IO ()
triggerFailHook = readIORef failHooks >>= sequence_

triggerHook :: Hook -> NGLessIO ()
triggerHook hook = do
    registered <- liftIO $ readIORef nglHooks
    forM_ registered $ \(h,act) ->
        when (h == hook) act
