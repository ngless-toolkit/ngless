{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    (
 samStats
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
    incV True result (fromEnum Total)
    incV (isAligned samLine) result (fromEnum Aligned)
    incV (isUnique samLine) result (fromEnum Unique) 
    incV (hasQual samLine) result (fromEnum LowQual) 


  