{- Copyright 2013-2016 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Conduit
    ( ByteLine(..)
    , conduitPossiblyCompressedFile
    , asyncMapC
    , linesC
    ) where

import qualified Data.ByteString as B
import qualified Control.Concurrent.Async as A
import qualified Data.Conduit as C
import           Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), ViewL(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception (evaluate)
import           Control.DeepSeq

import Utils.Utils (conduitPossiblyCompressedFile)

newtype ByteLine = ByteLine { unwrapByteLine :: B.ByteString }

linesC :: (Monad m) => C.Conduit B.ByteString m ByteLine
linesC = CB.lines =$= CL.map ByteLine

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
