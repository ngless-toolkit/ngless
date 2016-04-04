module Utils.Vector
 ( zeroVec
 , toFractions
 , unsafeIncrement
 , unsafeIncrement'
 , unsafeModify

 , binarySearch
 , binarySearchBy
 , binarySearchByRange
 , binarySearchByExact

 , binaryFindBy
 ) where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM


zeroVec n = VM.replicate n (0 :: Int)

unsafeIncrement :: (Num a, PrimMonad m, VM.Unbox a) => VM.MVector (PrimState m) a -> Int -> m ()
unsafeIncrement v i = unsafeIncrement' v i 1

unsafeIncrement' :: (Num a, PrimMonad m, VM.Unbox a) => VM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeIncrement' v i inc = unsafeModify v (+ inc) i

-- in vector >= 0.11, this function is already there:
unsafeModify :: (PrimMonad m, VM.Unbox a) => VM.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify v f i = do
    c <- VM.unsafeRead v i
    VM.unsafeWrite v i (f c)


toFractions :: (PrimMonad m, VM.Unbox a, Fractional a, Eq a, Num a) => VM.MVector (PrimState m) a -> m ()
toFractions v = do
    v' <- VU.unsafeFreeze v
    let total = VU.foldr1 (+) v'
        n = VM.length v
    when (total /= 0) $
        forM_ [0..n - 1] $ \i ->
            unsafeModify v (/ total) i

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
