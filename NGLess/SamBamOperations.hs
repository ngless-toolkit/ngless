{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    (
 samStats
    ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.Sam

samStats = computeStats . readAlignments

computeStats sams = runST $ do
    initVec <- zeroVec 3
    forM_ sams $ \x -> do
        update initVec x
    V.freeze initVec

update result samLine = do
    incV True result (fromEnum Total)
    incV (isAligned samLine) result (fromEnum Aligned)
    incV (isUnique samLine) result (fromEnum Unique) 


zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec

incV False _ _ = return ()
incV True v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)
    return ()
  