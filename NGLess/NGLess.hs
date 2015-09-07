{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NGLess
    ( NGLessIO
    , NGLess
    , NGError(..)
    , throwShouldNotOccur
    , throwScriptError
    , throwDataError
    , throwSystemError
    , throwGenericError
    , KwArgsValues
    , boolOrTypeError
    , symbolOrTypeError
    , testNGLessIO
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.Resource

import Language
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

data NGError = NGError !NGErrorType !T.Text
        deriving (Show, Eq)

type NGLessIO = ExceptT NGError (ResourceT IO)
type NGLess = Either NGError

type KwArgsValues = [(T.Text, NGLessObject)]

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO act = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)

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


boolOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m Bool
boolOrTypeError _ (NGOBool b) = return b
boolOrTypeError context val = throwScriptError (T.concat ["Expected a boolean (received ", T.pack . show $ val, ") in context '", asText context, "'"])

-- | If argument is a NGOBool, then unwraps it; else it raises a type error
symbolOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m T.Text
symbolOrTypeError _ (NGOSymbol s) = return s
symbolOrTypeError context val = throwScriptError (T.concat ["Expected a symbol (received ", T.pack . show $ val, ") in context '", asText context, "'"])
