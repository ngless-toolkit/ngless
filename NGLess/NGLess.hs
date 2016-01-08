{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NGLess
    ( NGLessIO
    , NGLess
    , NGError(..)
    , runNGLess
    , throwShouldNotOccur
    , throwScriptError
    , throwDataError
    , throwSystemError
    , throwGenericError
    , KwArgsValues
    , boolOrTypeError
    , symbolOrTypeError
    , stringOrTypeError
    , lookupBoolOrScriptError
    , lookupBoolOrScriptErrorDef
    , lookupStringOrScriptError
    , lookupStringOrScriptErrorDef
    , lookupStringListOrScriptError
    , lookupStringListOrScriptErrorDef
    , lookupIntegerOrScriptError
    , lookupIntegerOrScriptErrorDef
    , lookupSymbolOrScriptError
    , lookupSymbolOrScriptErrorDef
    , testNGLessIO
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.Resource

import NGLess.NGError

import Language
import Utils.StringLike


type NGLessIO = ExceptT NGError (ResourceT IO)
type NGLess = Either NGError

type KwArgsValues = [(T.Text, NGLessObject)]

runNGLess :: (MonadError NGError m) => Either NGError a -> m a
runNGLess (Left err) = throwError err
runNGLess (Right v) = return v

testNGLessIO :: NGLessIO a -> IO a
testNGLessIO act = do
        perr <- (runResourceT . runExceptT) act
        return (showError perr)
    where
        showError (Right a) = a
        showError (Left e) = error (show e)

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


lookupBoolOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m Bool
lookupBoolOrScriptError = requiredLookup lookupBoolOrScriptErrorDef

lookupBoolOrScriptErrorDef :: (MonadError NGError m) => m Bool -> String -> T.Text -> KwArgsValues -> m Bool
lookupBoolOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just v -> boolOrTypeError context v

lookupIntegerOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m Integer
lookupIntegerOrScriptError = requiredLookup lookupIntegerOrScriptErrorDef
lookupIntegerOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOInteger v) -> return v
    Just other -> throwScriptError (T.concat ["Expected an integer in argument ", name, " in context '", T.pack context, "' instead observed: ", T.pack . show $ other])

lookupSymbolOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m T.Text
lookupSymbolOrScriptError = requiredLookup lookupSymbolOrScriptErrorDef
lookupSymbolOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOSymbol s) -> return s
    Just other -> throwScriptError (T.concat ["Expected a symbol in argument ", name, " in context '", T.pack context, "' instead observed: ", T.pack . show $ other])


requiredLookup :: (MonadError NGError m) => (m a -> String-> T.Text -> KwArgsValues -> m a) -> String-> T.Text -> KwArgsValues -> m a
requiredLookup withDefaultLookup context name = withDefaultLookup errorAct context name
    where
        errorAct = throwScriptError (T.concat ["Could not find", name, " arguments (in context '", T.pack context, "')"])

