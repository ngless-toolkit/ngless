{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    ( samStats
    ) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as V

import Data.Sam
import VectorOperations

samStats :: BL.ByteString -> V.Vector Int
samStats = computeStats . readAlignments

computeStats :: [SamLine] -> V.Vector Int
computeStats sams = runST $ do
    vec <- zeroVec 4
    forM_ sams $ \samline -> do
        update vec samline
    V.freeze vec

update result samLine = do
        incV True Total
        incV (isAligned samLine) Aligned
        incV (isUnique samLine) Unique
        incV (hasQual samLine) LowQual
    where
        incV cond e = (when cond) (unsafeIncrement result (fromEnum e))

