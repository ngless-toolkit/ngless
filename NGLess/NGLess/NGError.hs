{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NGLess.NGError
    ( NGError(..)
    , throwShouldNotOccur
    , throwScriptError
    , throwDataError
    , throwSystemError
    , throwGenericError
    ) where

import qualified Data.Text as T
import           Control.DeepSeq
import           Control.Monad.Except

import Utils.StringLike

-- | An error in evaluating an ngless script
-- Normally, it's easier to use the function interface of 'throwShouldNotOccur' and friends
data NGErrorType =
    ShouldNotOccur -- ^ bug in ngless
    | ScriptError -- ^ bug in user script
    | DataError -- ^ bad input
    | SystemError -- ^ system/IO issue
    | GenericError -- ^ arbitrary error message
    deriving (Show, Eq)

instance NFData NGErrorType where
    rnf !_ = ()

data NGError = NGError !NGErrorType !T.Text
    deriving (Show, Eq)

instance NFData NGError where
    rnf !_ = ()

-- | Internal bug: user is requested to submit a bug report
throwShouldNotOccur :: (StringLike s, MonadError NGError m) => s -> m a
throwShouldNotOccur = throwError . NGError ShouldNotOccur . asText

-- | Script error: user can fix error by re-writing the script
throwScriptError :: (StringLike s, MonadError NGError m) => s -> m a
throwScriptError = throwError . NGError ScriptError . asText

-- | Data error: problem with input data
throwDataError :: (StringLike s, MonadError NGError m) => s -> m a
throwDataError = throwError . NGError DataError . asText

-- | System error: issues such as *subcommand failed* or *out of disk*
throwSystemError :: (StringLike s, MonadError NGError m) => s -> m a
throwSystemError = throwError . NGError SystemError . asText

-- | Generic error: any error message
throwGenericError :: (StringLike s, MonadError NGError m) => s -> m a
throwGenericError = throwError . NGError GenericError . asText

