{- Copyright 2014-2016 NGLess Authors
 - License: MIT
 -}


module Utils.ProgressBar
    ( mkProgressBar
    , updateProgressBar
    ) where

import Text.Printf
import System.IO
import Control.Monad

data ProgressBar = ProgressBar
    { cur :: Rational
    , width :: Int
    , pbarActive :: Bool
    } deriving (Eq, Show)


noProgress = ProgressBar (-1) (-1) False

-- | Redraw progress bar
updateProgressBar :: ProgressBar -- ^ previous progressbar
                  -> Rational -- ^ current fractional progress
                  -> IO ProgressBar -- ^ new progressbar
updateProgressBar bar _
    | not (pbarActive bar) = return bar
updateProgressBar bar progress = do
    when (percent progress /= percent (cur bar)) $ do
        let s = drawProgressBar (width bar) progress ++ " " ++ printPercentage progress
        putStr s
        putStr "\r"
    return $ bar { cur = progress }

-- | create a new 'ProgressBar' object
-- This function also checks if 'stdout' is a terminal device.
-- If it is not, then it returns a null progress bar,
-- one which does not draw on the screen.
mkProgressBar :: Int -> IO ProgressBar
mkProgressBar w = do
    isTerm <- hIsTerminalDevice stdout
    if isTerm
        then do
            hSetBuffering stdout NoBuffering
            return $ ProgressBar (-1) w True
        else return noProgress

drawProgressBar :: Int -> Rational -> String
drawProgressBar w progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral w)
        spaces = w - bars

percent :: Rational -> Int
percent = round . (* 1000)

printPercentage :: Rational -> String
printPercentage progress = printf "%6.1f%%" (fromRational (progress * 100) :: Double)

