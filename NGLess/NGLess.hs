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
    , stringOrTypeError
    , lookupStringOrScriptError
    , lookupStringOrScriptErrorDef
    , lookupStringListOrScriptError
    , lookupStringListOrScriptErrorDef
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

-- | If argument is a NGOString, then unwraps it; else it raises a type error
stringOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m T.Text
stringOrTypeError _ (NGOString s) = return s
stringOrTypeError context val = throwScriptError (T.concat ["Expected a string (received ", T.pack . show $ val, ") in context '", asText context, "'"])

lookupStringOrScriptErrorDef :: (MonadError NGError m) => m T.Text -> String -> T.Text -> KwArgsValues -> m T.Text
lookupStringOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOString s) -> return s
    Just other -> throwScriptError (T.concat ["Expected a string in argument ", name, " in context '", T.pack context, "' instead observed: ", T.pack . show $ other])

lookupStringOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m T.Text
lookupStringOrScriptError = requiredLookup lookupStringOrScriptErrorDef

lookupStringListOrScriptErrorDef :: (MonadError NGError m) => m [T.Text] -> String -> T.Text -> KwArgsValues -> m [T.Text]
lookupStringListOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOList ss) -> (stringOrTypeError context) `mapM` ss
    Just other -> throwScriptError (T.concat ["Expected a string in argument ", name, " in context '", T.pack context, "', instead saw ", T.pack . show $ other])

lookupStringListOrScriptError :: (MonadError NGError m) => String -> T.Text -> KwArgsValues -> m [T.Text]
lookupStringListOrScriptError = requiredLookup lookupStringListOrScriptErrorDef

requiredLookup :: (MonadError NGError m) => (m a -> String-> T.Text -> KwArgsValues -> m a) -> String-> T.Text -> KwArgsValues -> m a
requiredLookup withDefaultLookup context name = withDefaultLookup errorAct context name
    where
        errorAct = throwScriptError (T.concat ["Could not find", name, " arguments (in context '", T.pack context, "')"])

