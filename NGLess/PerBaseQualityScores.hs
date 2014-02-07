module PerBaseQualityScores
    (
        calculateStatistics,
        printHtmlStatisticsData,
        calcMean,
        calcPerc
    ) where

import Data.Map
import Data.Char
import PrintFastqBasicStats

-- JSON File Dest
dataFileName = "perbaseQualScoresData.js"
--

--constants
percentile50 = 0.5 :: Double
lowerQuartile = 0.25 :: Double
upperQuartile = 0.75 :: Double
--

--accUntilLim :: given lim, each position of the array is added until lim. Is returned the elem of the array in that position.
accUntilLim :: [Int] -> Int -> Int
accUntilLim array lim = accUntilLim' array 0 0
    where accUntilLim' [] acc _ = error ("ERROR: The accumulator has value" ++ show acc ++ " and is never higher than " ++ show lim )
          accUntilLim' (x:xs) acc offset' = (if (acc + x) >= lim then offset' else accUntilLim' xs (acc + x) (1 + offset'))

-- TODO: lQ, uQ, tenthPerc, ninetiethPerc
concatData :: (Double, Int, Int, Int) -> String -> Int -> String
concatData (mean, median, lq, uq) content bp =
    content ++ "{ \"bp\" :" ++ show bp ++
    ", \"mean\" :" ++ show mean ++
    ", \"median\" :" ++ show median ++
    ", \"Lower Quartile\" :" ++ show lq ++
    ", \"Upper Quartile\" :" ++ show uq ++
     "},\n"

createDataString stats = createDataString' stats "data = [\n " 0
    where createDataString' [] content _ = (content ++ "]\n")
          createDataString' (eachBp:xs) content bp = createDataString' xs (concatData eachBp content bp) (bp + 1)

printHtmlStatisticsData qCounts minChar destDir = writeFile (destDir ++ "/" ++ dataFileName) (createDataString statisticsData')
        where statisticsData' = calculateStatistics qCounts minChar

calculateStatistics qCounts minChar = Prelude.map (statistics encScheme') qCounts
    where encScheme' = offset (calculateEncoding minChar)

calcMean _ _ 0 = error "The total number of quality elements in the fastQ needs to be higher than 0"
calcMean keySet elemSet elemTotal = fromIntegral bpSum' / fromIntegral elemTotal
    where eachBpValue = zipWith (*) keySet elemSet
          bpSum' = foldl1 (+) eachBpValue


--calcPerc :: Given a specific percentil,  calculates it's results. Only being used for now to calculate the Median (percentile 50%)
calcPerc keySet elemSet elemTotal perc = keySet !! (accUntilLim elemSet val')
    where val' = (ceiling (fromIntegral elemTotal * perc) :: Int)


--statistics :: Calculates the Quality Statistics of a given FastQ.
statistics :: Fractional a => Int -> Map Char Int -> (a, Int, Int, Int)
statistics encScheme bpQualCount = (calcMean keySet'' elemSet' elemTotal' ,
                                   (subtract encScheme) $ calcPerc' percentile50,
                                   (subtract encScheme) $ calcPerc' lowerQuartile,
                                   (subtract encScheme) $ calcPerc' upperQuartile)
        where keySet = keys bpQualCount
              keySet' = Prelude.map (ord) keySet
              keySet'' = Prelude.map (subtract encScheme) keySet'
              elemSet' = elems bpQualCount
              elemTotal' = foldl1 (+) elemSet'
              calcPerc' = calcPerc keySet' elemSet' elemTotal'



