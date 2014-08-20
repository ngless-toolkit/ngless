{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    ( samStats
    ) where

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V

import Data.Sam
import VectorOperations

samStats = computeStats . readAlignments

computeStats sams = runST $ do
    initVec <- zeroVec 4
    forM_ sams $ \x -> do
        update initVec x
    V.freeze initVec

update result samLine = do
        incV True Total
        incV (isAligned samLine) Aligned
        incV (isUnique samLine) Unique
        incV (hasQual samLine) LowQual
    where
        incV cond e = (when cond) (unsafeIncrement result (fromEnum e))

