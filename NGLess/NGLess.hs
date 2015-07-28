{-# LANGUAGE OverloadedStrings #-}
module NGLess
    ( NGLessIO
    , NGError(..)
    , KwArgsValues
    , testNGLessIO
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import Data.String

import Language

-- For now just a string, but should become more descriptive later
data NGError = NGError !T.Text
        deriving (Show, Eq)

instance IsString NGError where
    fromString = NGError . T.pack

type NGLessIO = ExceptT NGError (ResourceT IO)
type KwArgsValues = [(T.Text, NGLessObject)]

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO act = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)
