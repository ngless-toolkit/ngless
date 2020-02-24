{- Copyright 2014-2020 NGLess Authors
 - License: MIT
 -}

module Utils.ProgressBar
    ( mkProgressBar
    , updateProgressBar
    ) where

import qualified Text.Printf as TP
import           System.IO (stdout, hFlush, hIsTerminalDevice)
import qualified Data.Time as Time

data ProgressBarData = ProgressBarData
    { pbarName :: String
    , cur :: Rational
    , lastUpdated :: Time.UTCTime
    , width :: Int
    } deriving (Eq, Show)

type ProgressBar = Maybe ProgressBarData


-- | Redraw progress bar
--
-- If the last update was less than 1 second ago, then nothing is updated
updateProgressBar :: ProgressBar -- ^ previous progressbar
                  -> Rational -- ^ current fractional progress
                  -> IO ProgressBar -- ^ new progressbar
updateProgressBar Nothing _ = return Nothing
updateProgressBar (Just bar) progress = do
    now <- Time.getCurrentTime
    if (now `Time.diffUTCTime` lastUpdated bar) > 1 && (percent progress /= percent (cur bar))
        then do
            let pmessage = drawProgressBar (width bar) progress ++ " " ++ printPercentage progress
                m = pbarName bar ++ pmessage ++ "\r"
            putStr m
            hFlush stdout
            return . Just $ bar { cur = progress, lastUpdated = now }
        else return (Just bar)

-- | create a new 'ProgressBar' object
--
-- This function also checks if 'stdout' is a terminal device. If it is not,
-- then it returns a null progress bar, one which does not draw on the screen.
mkProgressBar :: String -> Int -> IO ProgressBar
mkProgressBar name w = do
    isTerm <- hIsTerminalDevice stdout
    now <- Time.getCurrentTime
    return $! if isTerm
        then Just (ProgressBarData name (-1) now w)
        else Nothing

drawProgressBar :: Int -> Rational -> String
drawProgressBar w progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral w)
        spaces = w - bars

percent :: Rational -> Int
percent = round . (* 1000)

printPercentage :: Rational -> String
printPercentage progress = TP.printf "%6.1f%%" (fromRational (progress * 100) :: Double)

