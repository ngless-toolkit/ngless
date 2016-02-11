{- Copyright 2013-2016 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Conduit
    ( ByteLine(..)
    , conduitPossiblyCompressedFile
    , asyncMapC
    , linesC
    , groupC
    ) where

import qualified Data.ByteString as B
import qualified Control.Concurrent.Async as A
import qualified Data.Conduit as C
import           Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), ViewL(..))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception (evaluate)
import           Control.DeepSeq

import Utils.Utils (conduitPossiblyCompressedFile)

newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC = CB.lines =$= CL.map ByteLine

-- | This is like Data.Conduit.List.map, except that each element is processed
-- in a separate thread (up to maxSize can be queued up at any one time).
-- Results are evaluated to normal form (not WHNF!) to ensure that the
-- computation is fully evaluated before being yielded to the next conduit.
asyncMapC :: forall a m b . (MonadIO m, NFData b) => Int -> (a -> b) -> C.Conduit a m b
asyncMapC maxSize f = initLoop (0 :: Int) (Seq.empty :: Seq.Seq (A.Async b))
    where
        initLoop :: Int -> Seq.Seq (A.Async b) -> C.Conduit a m b
        initLoop size q
            | size == maxSize = loop q
            | otherwise = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                        v' <- sched v
                        initLoop (size + 1) (q |> v')
        sched :: a -> C.ConduitM a b m (A.Async b)
        sched v = liftIO . A.async . evaluate . force $ f v

        -- | yield all
        yAll :: Seq.Seq (A.Async b) -> C.Conduit a m b
        yAll q = case Seq.viewl q of
            EmptyL -> return ()
            v :< rest -> (liftIO (A.wait v) >>= C.yield) >> yAll rest

        loop :: Seq.Seq (A.Async b) -> C.Conduit a m b
        loop q = C.await >>= \case
                Nothing -> yAll q
                Just v -> do
                    v' <- sched v
                    case Seq.viewl q of
                        (r :< rest) -> do
                            C.yield =<< liftIO (A.wait r)
                            loop (rest |> v')
                        _ -> error "should never happen"

-- | groupC yields the input as groups of 'n' elements. If the input is not a
-- multiple of 'n', the last element will be incomplete
groupC :: (Monad m) => Int -> C.Conduit a m [a]
groupC n = loop n []
    where
        loop 0 ps = C.yield (reverse ps) >> loop n []
        loop c ps = C.await >>= \case
            Nothing -> unless (null ps) $ C.yield (reverse ps)
            Just p -> loop (c-1) (p:ps)

