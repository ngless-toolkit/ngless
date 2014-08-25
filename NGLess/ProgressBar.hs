module ProgressBar
    ( putProgressBar
    ) where

import System.IO
import Text.Printf

putProgressBar :: Int -> Rational -> IO ()
putProgressBar width progress =  putProgress $ drawProgressBar width progress ++ " " ++ printPercentage progress

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s

drawProgressBar :: Int -> Rational -> String
drawProgressBar width progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral width)
        spaces = width - bars

printPercentage :: Rational -> String
printPercentage progress = printf "%3d%%" (truncate (progress * 100) :: Int)

