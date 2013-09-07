module PerBaseQualityScores
    (
        calculateMean
    ) where

import Data.Map
import Data.Char
import PrintFastqBasicStats

calculateMean qCounts minChar = calculateMean' qCounts []
    where encScheme = offset (calculateEncoding minChar)
          calculateMean' [] meanResult = meanResult
          calculateMean' qcounts meanResult = calculateMean' (init qcounts) (mean (last qcounts) encScheme : meanResult)

mean bpQualCount encScheme = fromIntegral bpSum / fromIntegral total
                          where keySet = keys bpQualCount
                                elemSet = elems bpQualCount
                                total = foldl1 (+) elemSet
                                keySet' = Prelude.map (ord) keySet
                                keySet'' = Prelude.map (subtract encScheme) keySet'
                                eachBpValue = zipWith (*) keySet'' elemSet
                                bpSum = foldl1 (+) eachBpValue

