{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

module Utils.Vector
 ( unsafeIncrement
 , unsafeIncrement'

 , binarySearch
 , binarySearchBy
 , binarySearchByRange
 , binarySearchByExact

 , binaryFindBy

 , sortParallel

 , withVector
 ) where

import           Control.Monad (guard)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified Control.Concurrent.Async as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Vector.Algorithms.Intro (sortByBounds)

-- | increment by 1
unsafeIncrement :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> m ()
unsafeIncrement v i = unsafeIncrement' v i 1
{-# INLINE unsafeIncrement #-}

-- | increment by given value
unsafeIncrement' :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeIncrement' v i inc = VUM.unsafeModify v (+ inc) i
{-# INLINE unsafeIncrement' #-}

binarySearch :: (Ord a) => V.Vector a -> a -> Int
binarySearch v = binarySearchByRange 0 (V.length v) compare v

binarySearchByRange :: Int -> Int -> (a -> b -> Ordering) -> V.Vector a -> b -> Int
binarySearchByRange start end comp v target = loop start end
    where
        loop :: Int -> Int -> Int
        loop s e
            | s == e = s
            | otherwise = let mid = (s + e) `div` 2
                            in if comp (V.unsafeIndex v mid) target == LT
                                    then loop (mid + 1) e
                                    else loop s mid
binarySearchBy :: (a -> b -> Ordering) -> V.Vector a -> b -> Int
binarySearchBy comp v = binarySearchByRange 0 (V.length v) comp v

binarySearchByExact :: (a -> b -> Ordering) -> V.Vector a -> b -> Maybe Int
binarySearchByExact comp v target = do
    let ix = binarySearchBy comp v target
    guard (ix < V.length v)
    guard (comp (V.unsafeIndex v ix) target == EQ)
    return ix

binaryFindBy :: (a -> b -> Ordering) -> V.Vector a -> b -> Maybe a
binaryFindBy comp v target = do
    ix <- binarySearchByExact comp v target
    return $! V.unsafeIndex v ix


-- sort a vector in parallel threads
sortParallel :: (Ord e) =>
                        Int -- ^ nr of threads
                        -> VM.IOVector e -- ^ vector
                        -> IO ()
sortParallel n v = sortPByBounds n v 0 (VM.length v)

sortPByBounds :: (Ord e) => Int -> VM.IOVector e -> Int -> Int -> IO ()
sortPByBounds 1 v start end = sortByBounds compare v start end
sortPByBounds threads v start end
    | end - start < 1024 = sortByBounds compare v start end
    | otherwise = do
        mid <- pivot v start end
        k <- unstablePartition (< mid) v start end
        let t1 :: Int
            t1
                | k - start < 1024 = 1
                | end - k < 1024 = threads - 1
                | otherwise = threads `div` 2
            t2 = threads - t1
        A.concurrently_
            (sortPByBounds t1 v start k)
            (sortPByBounds t2 v k end)

pivot v start end = do
        a <- VM.read v start
        b <- VM.read v (end - 1)
        c <- VM.read v ((start + end) `div` 2)
        return $! median a b c
    where
        median a b c = if a <= b
            then min b c
            else max a c

unstablePartition f v start end = (+ start) <$> VGM.unstablePartition f (VM.unsafeSlice start (end - start) v)


withVector :: (MonadIO m, VUM.Unbox a) => VUM.IOVector a -> (VU.Vector a -> b) -> m b
withVector v f = liftIO $ do
    v' <- VU.unsafeFreeze v
    return $! f v'
