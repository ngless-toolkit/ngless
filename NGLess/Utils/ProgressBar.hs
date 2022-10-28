{- Copyright 2014-2022 NGLess Authors
 - License: MIT
 -}

module Utils.ProgressBar
    ( mkProgressBar
    , updateProgressBar
    ) where

import qualified Text.Printf as TP
import           System.IO (stdout, hIsTerminalDevice)
import qualified Data.Time as Time
import Control.Monad.IO.Class (liftIO)

import NGLess
import Output

data ProgressBarData = ProgressBarData
    { pbarName      :: String
    , cur           :: !Rational
    , lastUpdated   :: !Time.UTCTime
    , started       :: !Time.UTCTime
    , snapshot0     :: (Rational, Time.UTCTime)
    , width         :: !Int
    } deriving (Eq, Show)

type ProgressBar = Maybe ProgressBarData


-- | Redraw progress bar
--
-- If the last update was less than 1 second ago, then nothing is updated
updateProgressBar :: ProgressBar -- ^ previous progressbar
                  -> Rational -- ^ current fractional progress
                  -> NGLessIO ProgressBar -- ^ new progressbar
updateProgressBar Nothing _ = return Nothing
updateProgressBar (Just bar) progress = do
    now <- liftIO $ Time.getCurrentTime
    if (now `Time.diffUTCTime` lastUpdated bar) > 1 && (percent progress /= percent (cur bar))
        then do
            {- Estimate ETA
             -
             - 1. We ignore the first 10 seconds (because often there is a burst of initial activity)
             - 2. Then we take a snapshot (timestamp, progress at that point)
             - 3. Ignore the subsequent 10 seconds (unless at least 10% of the process is done)
             - 4. Estimate the speeed as the progress since the snapshot divided by the time
             - 5. Add 5% because users prefer over-estimated
             -}
            let elapsed = toRational $ Time.diffUTCTime now (started bar)
                (progressDelayed, startedDelayed) = snapshot0 bar
                elapsedDelayed = toRational $ Time.diffUTCTime now startedDelayed
                missing = (1 - progress) * elapsedDelayed / (progress - progressDelayed)

                eta :: String
                (eta, newDelayed) =
                    if
                        | elapsed < 10
                            -> ("no ETA yet", snapshot0 bar)
                        | elapsed == elapsedDelayed
                            -> ("no ETA yet", (progress, now))
                        | elapsed < 20 && progress < 0.1
                            -> ("no ETA yet", snapshot0 bar)
                        | otherwise
                            -> (TP.printf "ETA: %s" (showSecs $ 1.05 * missing)
                               ,snapshot0 bar)
            writeTransientMsg $ TP.printf "%s: %s %s (%s elapsed; %s)"
                            (pbarName bar)
                            (drawProgressBar (width bar) progress)
                            (showPercentage progress)
                            (showSecs elapsed)
                            eta
            return . Just $ bar { cur = progress, lastUpdated = now, snapshot0 = newDelayed }
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
        then Just (ProgressBarData name (-1) now now (0, now) w)
        else Nothing

drawProgressBar :: Int -> Rational -> String
drawProgressBar w progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral w)
        spaces = w - bars

percent :: Rational -> Int
percent = round . (* 1000)

showPercentage :: Rational -> String
showPercentage progress = TP.printf "%6.1f%%" (fromRational (progress * 100) :: Double)

showSecs :: Rational -> String
showSecs t =
    let
        secs = round t :: Integer
        secsr = secs `rem` 60
        mins = secs `div` 60
        minsr = mins `rem` 60
        hours = mins `div` 60
        hoursr = hours `rem` 24
        days = hours `div` 24
    in if
        | secs < 60 -> show secs ++ "s"
        | secs < 60 * 60 ->
                       TP.printf         "%02d:%02d"             mins  secsr
        | days == 0 -> TP.printf    "%02d:%02d:%02d"      hours  minsr secsr
        | otherwise -> TP.printf "%d-%02d:%02d:%02d" days hoursr minsr secsr

