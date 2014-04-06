

module ProgressBar
    ( 
    drawProgressBar,
    putProgress,
    drawPercentage
    ) where

import System.IO
import Text.Printf

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s

drawProgressBar :: Int -> Rational -> String
drawProgressBar width progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral width)
        spaces = width - bars

drawPercentage :: Rational -> String
drawPercentage progress = printf "%3d%%" (truncate (progress * 100) :: Int)

