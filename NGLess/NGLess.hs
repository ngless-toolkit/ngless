{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NGLess
    ( NGLessIO
    , NGError(..)
    , throwShouldNotOccurr
    , throwScriptError
    , throwDataError
    , throwSystemError
    , throwGenericError
    , KwArgsValues
    , testNGLessIO
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.Resource

import Language
import Utils.StringLike

-- For now just a string, but should become more descriptive later
data NGError =
    ShouldNotOccur !T.Text
    | ScriptError !T.Text
    | DataError !T.Text
    | SystemError !T.Text
    | GenericError !T.Text
        deriving (Show, Eq)

type NGLessIO = ExceptT NGError (ResourceT IO)
type KwArgsValues = [(T.Text, NGLessObject)]

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO act = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)

-- Internal bug: user is requested to submit a bug report
throwShouldNotOccurr :: (StringLike s, MonadError NGError m) => s -> m a
throwShouldNotOccurr = throwError . ShouldNotOccur . asText

throwScriptError :: (StringLike s, MonadError NGError m) => s -> m a
throwScriptError = throwError . ScriptError . asText

throwDataError :: (StringLike s, MonadError NGError m) => s -> m a
throwDataError = throwError . DataError . asText

throwSystemError :: (StringLike s, MonadError NGError m) => s -> m a
throwSystemError = throwError . SystemError . asText

throwGenericError :: (StringLike s, MonadError NGError m) => s -> m a
throwGenericError = throwError . GenericError . asText

