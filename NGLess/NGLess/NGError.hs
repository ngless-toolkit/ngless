{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
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
import           Control.Monad.Primitive
import           Control.Monad.Fail (MonadFail(..))
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Exception

-- This file should be a leaf in the import graph (i.e., not import any other NGLess modules)

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

instance Exception NGError

type NGLess = Either NGError

newtype NGLessIO a = NGLessIO { unwrapNGLessIO :: ResourceT IO a }
                        deriving (Functor, Applicative, Monad, MonadIO,
                        MonadResource, MonadThrow, MonadCatch, MonadMask)


instance MonadError NGError NGLessIO where
    throwError = liftIO . throwIO
    catchError = error "CATCH"

instance PrimMonad NGLessIO where
    type PrimState NGLessIO = PrimState IO
    primitive act = NGLessIO (primitive act)
    {-# INLINE primitive #-}

instance MonadUnliftIO NGLessIO where
    askUnliftIO = NGLessIO $ do
        u <- askUnliftIO
        return $ UnliftIO (\(NGLessIO act) -> unliftIO u act)

instance MonadFail NGLessIO where
    fail err = throwShouldNotOccur err

runNGLess :: (MonadError NGError m) => Either NGError a -> m a
runNGLess (Left err) = throwError err
runNGLess (Right v) = return v

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO (NGLessIO act) = runResourceT act

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

