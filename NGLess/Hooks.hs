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

{- Run if the script fails. Note that these hooks are in the IO Monad, not
 - NGLessIO! -}
registerFailHook :: IO () -> NGLessIO ()
registerFailHook act = liftIO $ modifyIORef failHooks (act:)

-- Run all fail hooks
triggerFailHook :: IO ()
triggerFailHook = readIORef failHooks >>= sequence_

-- Trigger the actions registered with the given hook
triggerHook :: Hook -> NGLessIO ()
triggerHook hook = do
    registered <- liftIO $ readIORef nglHooks
    forM_ registered $ \(h,act) ->
        when (h == hook) act
