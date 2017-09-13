module Utils.Vector
 ( unsafeIncrement
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
import qualified Control.Concurrent.SSem as SSem
import qualified Control.Concurrent.Async as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Vector.Algorithms.Intro (sortByBounds)

import Utils.Utils (mapMaybeM)

unsafeIncrement :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> m ()
unsafeIncrement v i = unsafeIncrement' v i 1

unsafeIncrement' :: (Num a, PrimMonad m, VUM.Unbox a) => VUM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeIncrement' v i inc = VUM.unsafeModify v (+ inc) i

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
sortParallel n v
    | n <= 0 = ioError (userError "sortParallel must be called with a strictly positive number of threads")
    | n == 1 = sortByBounds compare v 0 (VM.length v)
    | otherwise = do
        sem <- SSem.new (n - 1)
        sortPByBounds sem v 0 (VM.length v)

sortPByBounds :: (Ord e) => SSem.SSem -> VM.IOVector e -> Int -> Int -> IO ()
sortPByBounds sem v start end
    | end - start < 1024 = do
        sortByBounds compare v start end
    | otherwise = do
        mid <- pivot v start end
        k <- VGM.unstablePartition (< mid) v
        -- At this point, we have 1 thread allocated (the one which is running).
        -- We will attempt to acquire a second one. If successful, we can run
        -- the two blocks concurrently; otherwise, simply sort one after the
        -- other.
        let sortBlock1 = sortPByBounds sem v start k
            sortBlock2 = sortPByBounds sem v k end
        SSem.tryWait sem >>= \case
            Nothing -> sortBlock1 >> sortBlock2
            Just _ -> do
                A.concurrently_ sortBlock1 sortBlock2
                SSem.signal sem

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
