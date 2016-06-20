{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes, FlexibleContexts #-}

module Interpretation.Select
    ( executeSelect
    , executeMappedReadMethod
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Except
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Bits (Bits(..))
import Data.Conduit (($=), ($$), (=$=))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe

import Language
import FileManagement
import NGLess

import Utils.Samtools
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

-- readSamGroupsAsConduit :: (MonadIO m, MonadResource m) => FilePath -> C.Producer m [(SamLine, B.ByteString)]
-- The reason we cannot just use readSamGroupsC is that we want to get the unparsed ByteString on the side
readSamGroupsAsConduit fname =
        samBamConduit fname
            $= CB.lines
            =$= readSamLineOrDie
            =$= CL.groupBy groupLine
    where
        readSamLineOrDie = C.awaitForever $ \line ->
            case readSamLine line of
                Left err -> throwError err
                Right parsed -> C.yield (parsed,line)
        groupLine (SamHeader _,_) _ = False
        groupLine _ (SamHeader _,_) = False
        groupLine (s0,_) (s1,_) = samQName s0 == samQName s1


executeSelect :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeSelect (NGOMappedReadSet name fpsam ref) args = do
    conditions <- _parseConditions args
    (oname,ohandle) <- case lookup "__oname" args of
        Just (NGOString fname) -> let fname' = T.unpack fname in
                                    (fname',) <$> liftIO (openBinaryFile fname' WriteMode)
        Nothing -> openNGLTempFile fpsam "selected_" "sam"
        _ -> throwShouldNotOccur "Non-string argument in __oname variable"
    readSamGroupsAsConduit fpsam
        $= CL.map (_matchConditions conditions)
        =$= CL.concat
        =$= C.unlinesAscii
        $$ CB.sinkHandle ohandle
    liftIO (hClose ohandle)
    return (NGOMappedReadSet name oname ref)
executeSelect o _ = throwShouldNotOccur ("NGLESS type checking error (Select received " ++ show o ++ ")")

executeMappedReadMethod :: MethodName -> [SamLine] -> Maybe NGLessObject -> KwArgsValues -> NGLess NGLessObject
executeMappedReadMethod Mflag samlines (Just (NGOSymbol flag)) [] = do
        f <- getFlag flag
        return (NGOBool $ f samlines)
    where
        getFlag :: T.Text -> NGLess ([SamLine] -> Bool)
        getFlag "mapped" = return (any isAligned)
        getFlag "unmapped" = return (not . any isAligned)
        getFlag ferror = throwScriptError ("Flag " ++ show ferror ++ " is unknown for method flag")
executeMappedReadMethod Msome_match samlines (Just (NGOString target)) [] = return . NGOBool $ any ismatch samlines
    where
        ismatch :: SamLine -> Bool
        ismatch = (==target') . samRName
        target' = TE.encodeUtf8 target

executeMappedReadMethod Mpe_filter samlines Nothing [] = return . NGOMappedRead . filterPE $ samlines
executeMappedReadMethod Mfilter samlines Nothing kwargs = do
    minID <- lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "min_identity_pc" kwargs
    minMatchSize <- lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "min_match_size" kwargs
    action <- lookupSymbolOrScriptErrorDef (return "drop") "filter method" "action" kwargs
    let minIDD :: Double
        minIDD = fromInteger minID / 100.0
        matchIdentity' s = case matchIdentity s of
            Right v -> v
            Left _ -> 0.0
        matchSize' s = case matchSize s of
            Right v -> v
            Left _ -> 0
        okID s
            | minID == -1 = True
            | otherwise = matchIdentity' s >= minIDD
        okSize s
            | minMatchSize == -1 = True
            | otherwise = matchSize' s >= fromInteger minMatchSize
        acceptSamLine s = okID s && okSize s
        unmatchWhen c s
            | c s = unmatch s
            | otherwise = s
        unmatch samline = samline { samFlag = (samFlag samline .|. 4) `clearBit` 1, samRName = "*", samCigar = "*" }
        samlines'
            | action == "drop" = filter acceptSamLine samlines
            | action == "unmatch" = map (unmatchWhen acceptSamLine) samlines
            | otherwise = error "Unknown action in filter()"
    return (NGOMappedRead samlines')
executeMappedReadMethod Munique samlines Nothing [] = return . NGOMappedRead . mUnique $ samlines
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
isGroupUnique = allSame . map samRName
