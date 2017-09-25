{- Copyright 2017 NGLess Authors
 - License: MIT
 -}

module Utils.FindModules
    ( listKnownModules
    ) where


-- | This is imported inside Modules.hs
-- The only reason this is a separate module is because of limitations in GHC
-- and template haskell, it cannot be defined inline there.
import System.FilePath
import System.Directory
import Control.Monad.Extra
import Data.List (tails)
import Data.Maybe (mapMaybe)

listKnownModules :: IO [String]
listKnownModules = do
    -- Go up the directory stack until you find "NGLess.cabal.m4"
    cwd <- getCurrentDirectory
    let parents = fmap joinPath . tails $ splitDirectories cwd
    Just startpoint <- findM (doesFileExist . (</> "stack.yaml")) parents
    mapMaybe asModName <$> listDirectory (startpoint </> "Modules")


asModName m
    | takeExtension m == ".ngm" = Just (dropExtension m)
    | otherwise = Nothing
