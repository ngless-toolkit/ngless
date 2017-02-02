{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module NGLess.NGError
    ( NGError(..)
    , NGErrorType(..)
    , NGLessIO(..)
    , NGLess
    , runNGLess
    , testNGLessIO
    , throwShouldNotOccur
    , throwScriptError
    , throwDataError
    , throwSystemError
    , throwGenericError
    ) where

import           Control.DeepSeq
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           Control.Monad.Base

-- | An error in evaluating an ngless script
-- Normally, it's easier to use the function interface of 'throwShouldNotOccur' and friends
data NGErrorType =
    ShouldNotOccur -- ^ bug in ngless
    | ScriptError -- ^ bug in user script
    | DataError -- ^ bad input
    | SystemError -- ^ system/IO issue
    | GenericError -- ^ arbitrary error message
    | NoErrorExit -- ^ escape valve: ngless should immediately exit without error (arguably should not be overloading the error code)
    deriving (Show, Eq)

instance NFData NGErrorType where
    rnf !_ = ()

data NGError = NGError !NGErrorType !String
    deriving (Show, Eq)

instance NFData NGError where
    rnf !_ = ()

type NGLess = Either NGError

newtype NGLessIO a = NGLessIO { unwrapNGLessIO :: ExceptT NGError (ResourceT IO) a }
                        deriving (Functor, Applicative, Monad, MonadIO,
                        MonadError NGError, MonadResource, MonadThrow,
                        MonadBase IO)


newtype NGLessIOStM a = NGLessIOStM { unwrapNGLessIOStM :: StM (ExceptT NGError (ResourceT IO)) a }

instance MonadBaseControl IO NGLessIO where
    type StM NGLessIO a = NGLessIOStM a
    liftBaseWith f =  NGLessIO $ liftBaseWith (\q -> f (fmap NGLessIOStM . q . unwrapNGLessIO))
    restoreM = NGLessIO . restoreM . unwrapNGLessIOStM


runNGLess :: (MonadError NGError m) => Either NGError a -> m a
runNGLess (Left err) = throwError err
runNGLess (Right v) = return v

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO (NGLessIO act) = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)

-- | Internal bug: user is requested to submit a bug report
throwShouldNotOccur :: (MonadError NGError m) => String -> m a
throwShouldNotOccur = throwError . NGError ShouldNotOccur

-- | Script error: user can fix error by re-writing the script
throwScriptError :: (MonadError NGError m) => String -> m a
throwScriptError = throwError . NGError ScriptError

-- | Data error: problem with input data
throwDataError :: (MonadError NGError m) => String -> m a
throwDataError = throwError . NGError DataError

-- | System error: issues such as *subcommand failed* or *out of disk*
throwSystemError :: (MonadError NGError m) => String -> m a
throwSystemError = throwError . NGError SystemError

-- | Generic error: any error message
throwGenericError :: (MonadError NGError m) => String -> m a
throwGenericError = throwError . NGError GenericError

