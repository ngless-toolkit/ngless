module PerBaseQualityScores
    (
        calculateStatistics,
        printHtmlStatisticsData,
        calcMean,
        calcPerc,
        percentile50,
        lowerQuartile,
        upperQuartile
    ) where

import qualified Data.Vector.Unboxed as V

import System.FilePath.Posix

import PrintFastqBasicStats

import Control.Monad.ST
import Data.STRef
import Control.Monad

-- JSON File Dest
dataFileName = "perbaseQualScoresData.js"
--

--constants
percentile50 = 0.5 :: Double
lowerQuartile = 0.25 :: Double
upperQuartile = 0.75 :: Double
--

-- accUntilLim :: given lim, each position of the array is added until lim. 
-- Is returned the elem of the array in that position.
accUntilLim :: V.Vector Int -> Int -> Int
accUntilLim bps lim = do 
    let i = V.findIndex (>= lim) $ V.postscanl (+) 0 bps
    case i of
      Just v -> v
      Nothing -> error ("ERROR: Must exist a index with a accumulated value smaller than " ++ (show i))

concatData :: (Double, Int, Int, Int) -> String -> Int -> String
concatData (mean, median, lq, uq) content bp =
    content ++ "{ \"bp\" :" ++ show bp ++
    ", \"mean\" :" ++ show mean ++
    ", \"median\" :" ++ show median ++
    ", \"Lower Quartile\" :" ++ show lq ++
    ", \"Upper Quartile\" :" ++ show uq ++
     "},\n"

createDataString stats = createDataString' stats "data = [\n " 1
    where createDataString' [] content _ = (content ++ "]\n")
          createDataString' (eachBp:xs) content bp = createDataString' xs (concatData eachBp content bp) (bp + 1)

printHtmlStatisticsData qCounts minChar destDir = writeFile (destDir </> dataFileName) (createDataString statisticsData')
        where statisticsData' = calculateStatistics qCounts minChar

calculateStatistics qCounts minChar = Prelude.map (statistics encScheme') qCounts
    where encScheme' = offset (calculateEncoding minChar)

--statistics :: Calculates the Quality Statistics of a given FastQ.
statistics :: Fractional a => Int -> V.Vector Int -> (a, Int, Int, Int)
statistics encScheme bps = (calcMean bpSum' elemTotal' ,
                                   (calcPerc' percentile50) - encScheme,
                                   (calcPerc' lowerQuartile) - encScheme,
                                   (calcPerc' upperQuartile) - encScheme)
        where bpSum' = calcBPSum bps encScheme
              elemTotal' = V.sum bps
              calcPerc' x = calcPerc bps elemTotal' x

-- Calculates [('a',1), ('b',2)] = 0 + 'a' * 1 + 'b' * 2 . 
-- 'a' and 'b' minus encoding.
calcBPSum qc es = runST $ do
              n <- newSTRef 0
              let s = V.length qc
              forM_ [0..s - 1] $ \i -> do
                  modifySTRef n ((+) $ (i - es) * (V.unsafeIndex qc i))
              readSTRef n

-- calcMean :: Used to calculate the mean
calcMean _ 0 = error "The total number of quality elements in the fastQ needs to be higher than 0"
calcMean bpSum elemTotal = fromIntegral bpSum / fromIntegral elemTotal


--calcPerc :: Given a specific percentil,  calculates it's results.
calcPerc bps elemTotal perc = accUntilLim bps val'
    where val' = (ceiling (fromIntegral elemTotal * perc) :: Int) 

