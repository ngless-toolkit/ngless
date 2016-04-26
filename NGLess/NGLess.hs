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
import           Control.Monad.Except

import Language
import NGLess.NGError

type KwArgsValues = [(T.Text, NGLessObject)]


boolOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m Bool
boolOrTypeError _ (NGOBool b) = return b
boolOrTypeError context val = throwScriptError ("Expected a boolean (received " ++ show val ++  ") in context '" ++ context ++ "'")

-- | If argument is a NGOBool, then unwraps it; else it raises a type error
symbolOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m T.Text
symbolOrTypeError _ (NGOSymbol s) = return s
symbolOrTypeError context val = throwScriptError ("Expected a symbol (received " ++ show val ++ ") in context '" ++ context ++ "'")

-- | If argument is a NGOString, then unwraps it; else it raises a type error
stringOrTypeError :: (MonadError NGError m) => String -> NGLessObject -> m T.Text
stringOrTypeError _ (NGOString s) = return s
stringOrTypeError context val = throwScriptError ("Expected a string (received " ++ show val ++ ") in context '" ++ context ++ "'")

lookupStringOrScriptErrorDef :: (MonadError NGError m) => m T.Text -> String -> T.Text -> KwArgsValues -> m T.Text
lookupStringOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOString s) -> return s
    Just other -> throwScriptError ("Expected a string in argument " ++ T.unpack name ++ " in context '" ++ context ++ "' instead observed: " ++ show other)

lookupStringOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m T.Text
lookupStringOrScriptError = requiredLookup lookupStringOrScriptErrorDef

lookupStringListOrScriptErrorDef :: (MonadError NGError m) => m [T.Text] -> String -> T.Text -> KwArgsValues -> m [T.Text]
lookupStringListOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOList ss) -> (stringOrTypeError context) `mapM` ss
    Just other -> throwScriptError ("Expected a string in argument " ++ T.unpack name ++ " in context '" ++ context ++ "', instead saw " ++ show other)

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
    Just other -> throwScriptError ("Expected an integer in argument " ++ T.unpack name ++ " in context '" ++ context ++ "' instead observed: " ++ show other)

lookupSymbolOrScriptError :: (MonadError NGError m) => String-> T.Text -> KwArgsValues -> m T.Text
lookupSymbolOrScriptError = requiredLookup lookupSymbolOrScriptErrorDef
lookupSymbolOrScriptErrorDef defval context name args = case lookup name args of
    Nothing -> defval
    Just (NGOSymbol s) -> return s
    Just other -> throwScriptError ("Expected a symbol in argument " ++ T.unpack name ++ " in context '" ++ context ++ "' instead observed: " ++ show other)


requiredLookup :: (MonadError NGError m) => (m a -> String-> T.Text -> KwArgsValues -> m a) -> String-> T.Text -> KwArgsValues -> m a
requiredLookup withDefaultLookup context name = withDefaultLookup errorAct context name
    where
        errorAct = throwScriptError ("Could not find '" ++ T.unpack name ++ " arguments (in context '" ++ context ++ "')")

