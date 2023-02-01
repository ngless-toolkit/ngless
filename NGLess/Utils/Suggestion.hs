{- Copyright 2016-2023 NGLess Authors
 - License: MIT
 -}

module Utils.Suggestion
    ( Suggestion(..)
    , findSuggestion
    , suggestionMessage
    , checkFileReadable
    ) where

import qualified Data.Text as T
import qualified Text.EditDistance as TED
import Control.Monad (guard)
import Data.List.Extra (firstJust)
import Control.Applicative ((<|>))
import System.IO.Error (catchIOError)
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist, getDirectoryContents, getPermissions, readable)

data Suggestion = Suggestion
                        !T.Text
                        -- ^ Suggested text
                        !T.Text
                        -- ^ Reason for user
    deriving (Eq, Show)

-- | Given an erroneous legal input and a list of what the legal options were,
-- attempt to find a suggestion
--
-- See also: 'suggestionMessage'
findSuggestion :: T.Text
                    -- ^ What the user typed
                    -> [T.Text]
                    -- ^ List of legal options
                    -> Maybe Suggestion
                    -- ^ Suggestion if one is found, else Nothing
findSuggestion used possible = matchCase <|> prefixMatch <|> bestMatch
    where
        matchCase = firstJust matchCase1 possible
        matchCase1 s = do
            guard (T.toLower used == T.toLower s)
            return $! Suggestion s "note that ngless is case-sensitive"
        bestMatch = firstJust goodMatch1 possible
        prefixMatch = firstJust prefixMatch1 possible
        goodMatch1 s = do
            guard (dist used s <= 2)
            return $! Suggestion s "closest match"
        prefixMatch1 s = do
            guard (T.isPrefixOf used s)
            return $! Suggestion s "prefix match"

dist :: T.Text -> T.Text -> Int
dist a b = TED.levenshteinDistance TED.defaultEditCosts (T.unpack a) (T.unpack b)

-- | Like 'findSuggestion': If there is a possible suggestion for the given
-- input, returns a possible message for the user (or empty if no suggestion is
-- found)
suggestionMessage :: T.Text -> [T.Text] -> T.Text
suggestionMessage used valid = case findSuggestion used valid of
      Nothing -> ""
      Just (Suggestion suggestion reason) -> T.concat ["Did you mean '", suggestion, "' (", reason, ")?"]


-- | Check if the FilePath is readable, else return a suggestion
checkFileReadable :: FilePath -> IO (Maybe T.Text)
checkFileReadable fname = do
    let tfname = T.pack fname
    r <- doesFileExist fname
    if not r
        then do
            existing <- getDirectoryContents (takeDirectory fname)
                                    `catchIOError` (\_ -> return [])
            return . Just . T.concat $! ["File `", tfname, "` does not exist. ", suggestionMessage tfname (T.pack <$> existing)]
        else do
            p <- getPermissions fname
            return $! if not (readable p)
                        then Just (T.concat ["File `", tfname, "` is not readable (permissions problem)."])
                        else Nothing
