module PerBaseQualityScores
    (
        calculateStatistics
    ) where

import Data.Map
import Data.Char
import PrintFastqBasicStats

--constants
percentile50 = 0.5 :: Double
--

--accUntilLim :: given lim, each position of the array is added until lim. Is returned the elem of the array in that position.
accUntilLim :: [Int] -> Int -> Int
accUntilLim array lim = accUntilLim' array 0 0
    where accUntilLim' [] acc _ = error ("ERROR: The accumulator has value" ++ show acc ++ " and is never higher than " ++ show lim )
          accUntilLim' (x:xs) acc offset = (if (acc + x) >= lim then offset else accUntilLim' xs (acc + x) (1 + offset))


calculateStatistics qCounts minChar = calculateStatistics' qCounts []
    where encScheme' = offset (calculateEncoding minChar)
          calculateStatistics' [] bpStatsResult = bpStatsResult
          calculateStatistics' qcounts bpStatsResult = calculateStatistics' (init qcounts)
                                                                            (statistics (last qcounts) encScheme' : bpStatsResult)

calcMean keySet elemSet elemTotal = fromIntegral bpSum' / fromIntegral elemTotal
    where eachBpValue = zipWith (*) keySet elemSet
          bpSum' = foldl1 (+) eachBpValue
          

--calcPerc :: Given a specific percentil,  calculates it's results. Only being used for now to calculate the Median (percentile 50%)
calcPerc keySet elemSet elemTotal perc = keySet !! (accUntilLim elemSet val')
    where val' = (ceiling (fromIntegral elemTotal * perc) :: Int)
          

--statistics :: Calculates the Quality Statistics of a given FastQ.
statistics :: Fractional a => Map Char Int -> Int -> (a, Int)
statistics bpQualCount encScheme = (calcMean keySet'' elemSet' elemTotal' , (subtract encScheme) $ calcPerc keySet' elemSet' elemTotal' percentile50)
    where keySet = keys bpQualCount
          keySet' = Prelude.map (ord) keySet
          keySet'' = Prelude.map (subtract encScheme) keySet'
          elemSet' = elems bpQualCount
          elemTotal' = foldl1 (+) elemSet'



