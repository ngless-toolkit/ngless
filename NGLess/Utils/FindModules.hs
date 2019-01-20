{- Copyright 2017-2019 NGLess Authors
 - License: MIT
 -}

module Utils.FindModules
    ( listKnownModules
    ) where

-- | This is imported inside Modules.hs
-- The only reason this is a separate module is because of limitations in GHC
-- and template haskell, it cannot be defined inline there.
import System.FilePath ((</>), joinPath, splitDirectories, takeExtension, dropExtension)
import System.Directory (getCurrentDirectory, doesFileExist, listDirectory)
import Control.Monad.Extra (findM)
import Data.List (tails)
import Data.Maybe (mapMaybe)

listKnownModules :: IO [String]
listKnownModules = do
    -- Go up the directory stack until you find "stack.yaml"
    cwd <- getCurrentDirectory
    let parents = fmap joinPath . tails $ splitDirectories cwd
    findM (doesFileExist . (</> "stack.yaml")) parents >>= \case
        Just startpoint -> mapMaybe asModName <$> listDirectory (startpoint </> "Modules")
        Nothing -> error "Running in wrong directory?"


asModName m
    | takeExtension m == ".ngm" = Just (dropExtension m)
    | otherwise = Nothing
