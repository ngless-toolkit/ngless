{- Copyright 2014-2019 NGLess Authors
 - License: MIT
 -}

module Utils.ProgressBar
    ( mkProgressBar
    , updateProgressBar
    ) where

import Text.Printf
import System.IO
import Control.Monad (when)

data ProgressBar = ProgressBar
    { pbarName :: String
    , cur :: Rational
    , width :: Int
    , pbarActive :: Bool
    } deriving (Eq, Show)


noProgress :: ProgressBar
noProgress = ProgressBar "" (-1) (-1) False

-- | Redraw progress bar
updateProgressBar :: ProgressBar -- ^ previous progressbar
                  -> Rational -- ^ current fractional progress
                  -> IO ProgressBar -- ^ new progressbar
updateProgressBar bar _
    | not (pbarActive bar) = return bar
updateProgressBar bar progress = do
    when (percent progress /= percent (cur bar)) $ do
        let pmessage = drawProgressBar (width bar) progress ++ " " ++ printPercentage progress
            m = pbarName bar ++ pmessage ++ "\r"
        putStr m
        hFlush stdout
    return $ bar { cur = progress }

-- | create a new 'ProgressBar' object
-- This function also checks if 'stdout' is a terminal device.
-- If it is not, then it returns a null progress bar,
-- one which does not draw on the screen.
mkProgressBar :: String -> Int -> IO ProgressBar
mkProgressBar name w = do
    isTerm <- hIsTerminalDevice stdout
    return $! if isTerm
        then ProgressBar name (-1) w True
        else noProgress

drawProgressBar :: Int -> Rational -> String
drawProgressBar w progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral w)
        spaces = w - bars

percent :: Rational -> Int
percent = round . (* 1000)

printPercentage :: Rational -> String
printPercentage progress = printf "%6.1f%%" (fromRational (progress * 100) :: Double)

