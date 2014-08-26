module ProgressBar
    ( mkProgressBar
    , updateProgressBar
    ) where

import Text.Printf
import System.IO
import Control.Monad

data ProgressBar = ProgressBar
    { cur :: Rational
    , width :: Int
    } deriving (Eq, Show)

updateProgressBar :: ProgressBar -> Rational -> IO ProgressBar
updateProgressBar bar progress = do
    when ((percent progress) /= percent (cur bar)) $ do
        let s = drawProgressBar (width bar) progress ++ " " ++ printPercentage progress
        putStr s
        putStr "\r"
    return $ bar { cur = progress }

mkProgressBar :: Int -> IO ProgressBar
mkProgressBar w = do
    hSetBuffering stdout NoBuffering
    return $ ProgressBar (-1) w

drawProgressBar :: Int -> Rational -> String
drawProgressBar w progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral w)
        spaces = w - bars

percent :: Rational -> Int
percent = round . (* 1000)

printPercentage :: Rational -> String
printPercentage progress = printf "%6.1f%%" (fromRational (progress * 100) :: Double)

