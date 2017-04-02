module Utils.Vector
 ( zeroVec
 , toFractions
 , unsafeIncrement
 , unsafeIncrement'

 , binarySearch
 , binarySearchBy
 , binarySearchByRange
 , binarySearchByExact

 , binaryFindBy

 , sortParallel
 , mergeSorted

 , withVector
 ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import qualified Control.Concurrent.Async as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Vector.Algorithms.Intro (sortByBounds)

import Utils.Utils

zeroVec n = VUM.replicate n (0 :: Int)

unsafeIncrement :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> m ()
unsafeIncrement v i = unsafeIncrement' v i 1

unsafeIncrement' :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeIncrement' v i inc = VUM.unsafeModify v (+ inc) i

toFractions :: (PrimMonad m, VUM.Unbox a, Fractional a, Eq a) => VUM.MVector (PrimState m) a -> m ()
toFractions v = do
    v' <- VU.unsafeFreeze v
    let total = VU.foldr1 (+) v'
        n = VGM.length v
    when (total /= 0) $
        forM_ [0..n - 1] $ \i ->
            VUM.unsafeModify v (/ total) i

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



sortParallel :: (Ord e) => Int -> VM.IOVector e -> IO ()
sortParallel n v = sortPByBounds n v 0 (VM.length v)

sortPByBounds :: (Ord e) => Int -> VM.IOVector e -> Int -> Int -> IO ()
sortPByBounds 1 v start end = sortByBounds compare v start end
sortPByBounds threads v start end
    | end - start < 1024 = sortByBounds compare v start end
    | otherwise = do
        mid <- pivot v start end
        k <- VGM.unstablePartition (< mid) v
        let t1 :: Int
            t1
                | k - start < 1024 = 1
                | end - k < 1024 = threads - 1
                | otherwise = threads `div` 2
            t2 = threads - t1
        void $ A.concurrently
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



mergeSorted :: (Ord a) => [V.Vector a] -> V.Vector a
mergeSorted = mergeSorted' . filter (not . null)
mergeSorted' [] = V.empty
mergeSorted' [v] = v
mergeSorted' vs = V.create $ do
    let n = sum (map V.length vs)
        vsv = V.fromList vs
    r <- VM.new n
    pos <- VUM.replicate (V.length vsv) (0 :: Int)
    forM_ [0 .. n - 1] $ \ri -> do
        (val, i) <- minimum <$> (flip mapMaybeM [0 .. VUM.length pos - 1] $ \j -> do
            p <- VUM.read pos j
            return $! if p >= V.length (vsv V.! j)
                then Nothing
                else Just ((vsv V.! j) V.! p, j))
        VUM.modify pos (+1) i
        VM.write r ri val
    return r

withVector :: (MonadIO m, VUM.Unbox a) => VUM.IOVector a -> (VU.Vector a -> b) -> m b
withVector v f = liftIO $ do
    v' <- VU.unsafeFreeze v
    return $! f v'
