{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Interpretation.Select
    ( executeSelect
    , executeMappedReadMethod
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Except
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Bits (Bits(..))
import Data.Conduit ((=$=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Either.Combinators
import Data.Maybe

import FileManagement
import FileOrStream
import Language
import NGLess

import Data.Sam
import Utils.Utils
import Utils.Conduit

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
            (cs, []) -> return $! KeepIf cs
            ([], cs) -> return $! DropIf cs
            (_, _) -> throwScriptError "To select, you cannot use both keep_if and drop_if"
    where
        asSC :: NGLessObject -> NGLessIO SelectCondition
        asSC (NGOSymbol "mapped") = return SelectMapped
        asSC (NGOSymbol "unmapped") = return SelectUnmapped
        asSC (NGOSymbol "unique") = return SelectUnique
        asSC c = throwShouldNotOccur ("Check failed.  Should not have seen this condition: '" ++ show c ++ "'")

_matchConditions :: MatchCondition -> [(SamLine,B.ByteString)] -> [B.ByteString]
_matchConditions _ [(SamHeader _,line)] = [line]
_matchConditions (DropIf []) slines = map snd slines
_matchConditions (DropIf (c:cs)) slines = _matchConditions (DropIf cs) (_drop1 c slines)

_matchConditions (KeepIf []) slines = map snd slines
_matchConditions (KeepIf (c:cs)) slines = _matchConditions (KeepIf cs) (_keep1 c slines)

_drop1 SelectUnmapped = filter (isAligned . fst)
_drop1 SelectMapped = filter (not . isAligned . fst)
_drop1 SelectUnique = \g -> if isGroupUnique (map fst g) then [] else g

_keep1 SelectMapped = filter (isAligned . fst)
_keep1 SelectUnmapped = filter (not . isAligned . fst)
_keep1 SelectUnique = \g -> if isGroupUnique (map fst g) then g else []

-- readSamGroupsAsConduit :: (MonadIO m, MonadResource m) => FileOrStream -> C.Producer m [(SamLine, B.ByteString)]
-- The reason we cannot just use readSamGroupsC is that we want to get the unparsed ByteString on the side
readSamGroupsAsConduit istream paired =
        istream
            =$= readSamLineOrDie
            =$= CL.groupBy groupLine
    where
        readSamLineOrDie = C.awaitForever $ \(ByteLine line) ->
            case readSamLine line of
                Left err -> throwError err
                Right parsed -> C.yield (parsed,line)
        groupLine (SamHeader _,_) _ = False
        groupLine _ (SamHeader _,_) = False
        groupLine (s0,_) (s1,_)
            | paired = samQName s0 == samQName s1
            | otherwise = samQName s0 == samQName s1 && isFirstInPair s0 == isFirstInPair s1


executeSelect :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSelect (NGOMappedReadSet name istream ref) args = do
    paired <- lookupBoolOrScriptErrorDef (return True) "select" "paired" args
    conditions <- _parseConditions args
    let (fpsam, istream') = asSamStream istream
        stream =
            readSamGroupsAsConduit istream' paired
                =$= CL.map (_matchConditions conditions)
                =$= CL.concat
        out = Stream ("selected_" ++ takeBaseNameNoExtensions fpsam ++ ".sam") (stream =$= CL.map ByteLine)
    return $! NGOMappedReadSet name out ref
executeSelect o _ = throwShouldNotOccur ("NGLESS type checking error (Select received " ++ show o ++ ")")

data FilterAction = FADrop | FAUnmatch
    deriving (Eq)

executeMappedReadMethod :: MethodName -> [SamLine] -> Maybe NGLessObject -> KwArgsValues -> NGLess NGLessObject
executeMappedReadMethod (MethodName "flag") samlines (Just (NGOSymbol flag)) [] = do
        f <- getFlag flag
        return (NGOBool $ f samlines)
    where
        getFlag :: T.Text -> NGLess ([SamLine] -> Bool)
        getFlag "mapped" = return (any isAligned)
        getFlag "unmapped" = return (not . any isAligned)
        getFlag ferror = throwScriptError ("Flag " ++ show ferror ++ " is unknown for method flag")
executeMappedReadMethod (MethodName "some_match") samlines (Just (NGOString target)) [] = return . NGOBool $ any ismatch samlines
    where
        ismatch :: SamLine -> Bool
        ismatch = (==target') . samRName
        target' = TE.encodeUtf8 target


executeMappedReadMethod (MethodName "pe_filter") samlines Nothing [] = return . NGOMappedRead . filterPE $ samlines
executeMappedReadMethod (MethodName "filter") samlines Nothing kwargs = do
    minID <- lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "min_identity_pc" kwargs
    minMatchSize <- lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "min_match_size" kwargs
    reverseTest <- lookupBoolOrScriptErrorDef (return False) "filter method" "reverse" kwargs
    action <- lookupSymbolOrScriptErrorDef (return "drop") "filter method" "action" kwargs >>= \case
        "drop" -> return FADrop
        "unmatch" -> return FAUnmatch
        other -> throwScriptError ("unknown action in filter(): `" ++ T.unpack other ++"`.\nAllowed values are:\n\tdrop\n\tunmatch\n\tkeep\n")
    let minIDD :: Double
        minIDD = fromInteger minID / 100.0
        okID
            | minID == -1 = const True
            | otherwise = \s -> fromRight 0.0 (matchIdentity s) >= minIDD
        okSize
            | minMatchSize == -1 = const True
            | otherwise = \s -> fromRight 0 (matchSize s) >= fromInteger minMatchSize
        rawTest s = okID s && okSize s
        passTest
            | reverseTest = not . rawTest
            | otherwise = rawTest
        unmatchUnless c s
            | not (c s) = unmatch s
            | otherwise = s
        unmatch samline = samline { samFlag = (samFlag samline .|. 4) `clearBit` 1, samRName = "*", samCigar = "*" }
        samlines' = case action of
            FADrop -> filter passTest samlines
            FAUnmatch -> map (unmatchUnless passTest) samlines
    return (NGOMappedRead samlines')
executeMappedReadMethod (MethodName "unique") samlines Nothing [] = return . NGOMappedRead . mUnique $ samlines
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
        isMatch target other = isNegative other && samRName target == samRName other

mUnique :: [SamLine] -> [SamLine]
mUnique slines
    | isGroupUnique slines = slines
    | otherwise = []

isGroupUnique :: [SamLine] -> Bool
isGroupUnique [] = True
isGroupUnique [_] = True
isGroupUnique [f,s] = (isFirstInPair f /= isFirstInPair s) &&
                            (not (isAligned f && isAligned s) || (samRName f == samRName s))
isGroupUnique _ = False

