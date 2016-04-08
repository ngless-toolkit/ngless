{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module Utils.Suggestion
    ( Suggestion(..)
    , findSuggestion
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Alignment (editDistance)
import           Control.Monad
import           Control.Applicative

data Suggestion = Suggestion !T.Text !T.Text
    deriving (Eq, Show)

findSuggestion :: T.Text -> [T.Text] -> Maybe Suggestion
findSuggestion used possible = matchCase <|> bestMatch
    where
        matchCase = msum (map matchCase1 possible)
        matchCase1 s = do
            guard (T.toLower used == T.toLower s)
            return $! Suggestion s "note that ngless is case-sensitive"
        bestMatch = msum (map goodMatch1 possible)
        goodMatch1 s = do
            guard (dist used s <= 2)
            return $! Suggestion s "closest match"

dist :: T.Text -> T.Text -> Int
dist a b = editDistance (T.encodeUtf8 a) (T.encodeUtf8 b)


