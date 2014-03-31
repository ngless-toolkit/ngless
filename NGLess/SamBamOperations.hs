{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module SamBamOperations
    (
	samToBam
    ) where


import InvokeExternalProgs

samToBam :: FilePath -> IO ()
samToBam fp = convertSamToBam' fp


{-

samStats :: FilePath -> IO (Int)
samstats = calcStats . iterateSam

iterateSam :: FilePath -> [Bam.Bam1]
iterateSam fp = iterateSam' $ Bam.openTamInFile fp
	where iterateSam' handle = do
			bam' <- Bam.get1 handle
			case bam' of
				Maybe a -> a : iterateSam' handle
				Nothing -> []
-}