{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utils.Utils
    ( lookupWithDefault
    , maybeM
    , mapMaybeM
    , uniq
    , readProcessErrorWithExitCode
    , allSame
    , passthrough
    , moveOrCopy
    ) where

import Control.Monad
import Control.Concurrent
import System.IO
import System.Process

import System.Directory
import System.IO.Error
import Control.Exception
import GHC.IO.Exception (IOErrorType(..))

import Data.List (group)
import Data.Maybe (fromMaybe, catMaybes)

lookupWithDefault :: Eq b => a -> b -> [(b,a)] -> a
lookupWithDefault def key values = fromMaybe def $ lookup key values

uniq :: Eq a => [a] -> [a]
uniq = map head . group

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (e:es) = all (==e) es


readProcessErrorWithExitCode cp = do
    (_, _, Just herr, jHandle) <-
        createProcess cp { std_err = CreatePipe }
    err <- hGetContents herr
    -- In a separate thread, consume all the error input
    -- the same pattern is used in the implementation of
    -- readProcessWithErrorCode (which cannot be used here as we want to
    -- use `hout` for stdout)
    void . forkIO $ void (evaluate (length err))
    exitCode <- waitForProcess jHandle
    hClose herr
    return (err, exitCode)

maybeM :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
maybeM ma f = ma >>= \case
    Nothing -> return Nothing
    Just a -> f a

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs


-- | passthrough applies the function 'f' and then return its argument again
passthrough :: (Monad m) => (a -> m ()) -> a -> m a
passthrough f a = f a >> return a

moveOrCopy :: FilePath -> FilePath -> IO ()
moveOrCopy oldfp newfp = renameFile oldfp newfp `catch` (\e -> case ioeGetErrorType e of
            UnsupportedOperation -> copyFile oldfp newfp
            _ -> ioError e)
