{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE CPP #-}

module Utils.Utils
    ( lookupWithDefault
    , maybeM
    , mapMaybeM
    , fmapMaybeM
    , findM
    , uniq
    , allSame
    , passthrough
    , moveOrCopy
    , secondM
    , dropEnd
    , withOutputFile
    ) where

import System.Directory
import System.IO.Error
import Control.Exception
import GHC.IO.Exception (IOErrorType(..))

import Data.List (group)
import Data.Maybe (fromMaybe, catMaybes)
#ifdef WINDOWS
import           System.AtomicWrite.Internal (tempFileFor, closeAndRename)
#else
import           System.IO.SafeWrite (withOutputFile)
#endif

-- This module should not import from other NGLess modules

{- This module is a grab bag of utility functions
 -}

-- | lookup with a default if the key is not present in the association list
lookupWithDefault :: Eq b => a -> b -> [(b,a)] -> a
lookupWithDefault def key values = fromMaybe def $ lookup key values
{-# INLINE lookupWithDefault #-}

-- | equivalent to the Unix command 'uniq'
uniq :: Eq a => [a] -> [a]
uniq = map head . group

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (e:es) = all (==e) es
{-# INLINE allSame #-}

maybeM :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
maybeM ma f = ma >>= \case
    Nothing -> return Nothing
    Just a -> f a

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing = return Nothing
fmapMaybeM f (Just v) = Just <$> f v


-- | passthrough applies the function 'f' and then return its argument again
passthrough :: (Monad m) => (a -> m ()) -> a -> m a
passthrough f a = f a >> return a

-- | move a file if possible; otherwise copy
moveOrCopy :: FilePath -> FilePath -> IO ()
moveOrCopy oldfp newfp = renameFile oldfp newfp `catch` (\e -> case ioeGetErrorType e of
            UnsupportedOperation -> copyFile oldfp newfp
            _ -> ioError e)

-- | Monadic version of find: returns the result of the first application of
-- the argument which is not 'Nothing' or, if all applications fail, return
-- 'Nothing'
findM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
findM [] _ = return Nothing
findM (x:xs) f = f x >>= \case
    Nothing -> findM xs f
    val -> return val

secondM :: Monad m => (a -> m b) -> (c,a) -> m (c,b)
secondM f (a,c) = (a,) <$> f c
{-# INLINE secondM #-}

dropEnd :: Int -> [a] -> [a]
dropEnd v a = take (length a - v) a -- take of a negative is the empty sequence, which is correct in this case
{-# INLINE dropEnd #-}

#ifdef WINDOWS
-- Windows-compatible reimplementation of safeio's withOutputFile
-- (Note that this is not as complete as safeio as it does not sync the
-- directory).
withOutputFile :: FilePath -> (Handle -> IO a) -> IO a
withOutputFile fp act = do
    (fp', h) <- tempFileFor fp
    act h
    closeAndRename h fp' fp
#endif
