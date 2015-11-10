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
import Data.Maybe

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
_matchConditions _ (SamHeader _) = True
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
    samcontents <- liftIO (BL.lines <$> BL.readFile fpsam)
    forM_ samcontents $ \line -> do
        parsed <- runNGLess (readSamLine line)
        when (_matchConditions conditions parsed) $
            liftIO (BL.hPut ohand line >> BL.hPut ohand "\n")
    liftIO (hClose ohand)
    return (NGOMappedReadSet oname ref)
executeSelect o _ = throwShouldNotOccur ("NGLESS type checking error (Select received " ++ show o ++ ")")

executeMappedReadMethod :: MethodName -> [SamLine] -> Maybe NGLessObject -> KwArgsValues -> NGLess NGLessObject
executeMappedReadMethod Mflag samlines (Just (NGOSymbol flag)) [] = do
        f <- getFlag flag
        return (NGOBool $ f samlines)
    where
        getFlag "mapped" = return (any isAligned)
        getFlag "unmapped" = return (not . any isAligned)
        getFlag ferror = throwScriptError ("Flag " ++ show ferror ++ " is unknown for method flag")
executeMappedReadMethod Mpe_filter samlines Nothing [] = return . NGOMappedRead . filterPE $ samlines
executeMappedReadMethod m self arg kwargs = throwShouldNotOccur ("Method " ++ show m ++ " with self="++show self ++ " arg="++ show arg ++ " kwargs="++show kwargs ++ " is not implemented")

filterPE :: [SamLine] -> [SamLine]
filterPE slines = (filterPE' . filter isAligned) slines
    where
        filterPE' [] = []
        filterPE' (sl:sls)
            | isPositive sl = case findMatch sl slines of
                    Just sl2 -> sl:sl2:filterPE' sls
                    Nothing -> filterPE' sls
            | otherwise = filterPE' sls
        findMatch target = listToMaybe . filter (isMatch target)
        isMatch target other = isNegative other && (samRName target) == (samRName other)

