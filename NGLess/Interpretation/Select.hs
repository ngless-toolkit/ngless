{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Interpretation.Select
    ( executeSelect
    , executeMappedReadMethod
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import System.IO
import qualified Data.Text as T

import Language
import FileManagement
import NGLess

import Utils.Utils
import Data.Sam

data SelectCondition = SelectMapped | SelectUnmapped | SelectUnique
    deriving (Eq, Show)

data MatchCondition = KeepIf [SelectCondition] | DropIf [SelectCondition]
    deriving (Eq, Show)

_parseConditions :: KwArgsValues -> NGLessIO MatchCondition
_parseConditions args = do
        let NGOList keep_if = lookupWithDefault (NGOList []) "keep_if" args
            NGOList drop_if = lookupWithDefault (NGOList []) "drop_if" args
        keep_if' <- mapM asSC keep_if
        drop_if' <- mapM asSC drop_if
        case (keep_if', drop_if') of
            (cs, []) -> return (KeepIf cs)
            ([], cs) -> return (DropIf cs)
            (_, _) -> throwScriptError ("To select, you cannot use both keep_if and drop_if" :: String)
    where
        asSC (NGOSymbol "mapped") = return SelectMapped
        asSC (NGOSymbol "unmapped") = return SelectUnmapped
        asSC (NGOSymbol "unique") = return SelectUnique
        asSC c = throwShouldNotOccur ("Check failed.  Should not have seen this condition: '" ++ show c ++ "'")

_matchConditions :: MatchCondition -> SamLine -> Bool
_matchConditions (DropIf drop_if) samline = none (_match1 samline) drop_if
    where none f = not . any f
_matchConditions (KeepIf keep_if) samline = all (_match1 samline) keep_if

_match1 samline SelectMapped = isAligned samline
_match1 samline SelectUnmapped = not $ isAligned samline
_match1 samline SelectUnique = isUnique samline

executeSelect :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSelect (NGOMappedReadSet fpsam ref) args = do
    conditions <- _parseConditions args
    (oname,ohand) <- case lookup "__oname" args of
        Just (NGOString fname) -> let fname' = T.unpack fname in
                                    (fname',) <$> liftIO (openBinaryFile fname' WriteMode)
        Nothing -> openNGLTempFile fpsam "selected_" "sam"
        _ -> throwShouldNotOccur ("Non-string argument in __oname variable" :: T.Text)
    liftIO $ do
        samcontents <- BL.lines <$> BL.readFile fpsam
        forM_ samcontents $ \line ->
            when (BL.take 1 line == "@" ||
                _matchConditions conditions (readSamLine line))
                    (BL.hPut ohand line >> BL.hPut ohand "\n")
        hClose ohand
        return (NGOMappedReadSet oname ref)
executeSelect o _ = throwShouldNotOccur ("NGLESS type checking error (Select received " ++ show o ++ ")")

executeMappedReadMethod :: MethodName -> SamLine -> Maybe NGLessObject -> KwArgsValues -> NGLess NGLessObject
executeMappedReadMethod Mflag samline (Just (NGOSymbol flag)) [] = do
        f <- getFlag flag
        return (NGOBool $ f samline)
    where
        getFlag "mapped" = return isAligned
        getFlag "unmapped" = return (not . isAligned)
        getFlag ferror = throwScriptError ("Flag " ++ show ferror ++ " is unknown for method flag")
executeMappedReadMethod Mscore samline (Just (NGOSymbol scoref)) [] = case scoref of
    "identity_pc" -> do
        identity <- matchIdentity samline
        return . NGOInteger . round $ 100 * identity
    _ -> throwScriptError ("Unknown score function '"++T.unpack scoref++"'")
executeMappedReadMethod m self arg kwargs = throwShouldNotOccur ("Method " ++ show m ++ " with self="++show self ++ " arg="++ show arg ++ " kwargs="++show kwargs ++ " is not implemented")
