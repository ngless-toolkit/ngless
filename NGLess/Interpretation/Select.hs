{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}

module Interpretation.Select
    ( executeSelect
    , executeMappedReadMethod
    , splitSamlines3
    , fixCigar
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BB
import qualified Data.Vector as V
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit ((.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Bits (Bits(..))
import           Control.Monad.Except (throwError)
import           Data.Either.Combinators (fromRight)
import           Data.List (foldl', find)
import           Data.Either.Extra (eitherToMaybe)
import           Data.Tuple.Extra (fst3)
import           Data.Ratio (Ratio, (%))

import Data.Maybe

import Data.Sam
import FileManagement
import FileOrStream
import Language
import Output

import NGLess.NGLEnvironment
import NGLess

import Utils.Conduit

data SelectCondition = SelectMapped | SelectUnmapped | SelectUnique
    deriving (Eq, Show)

data MatchCondition = KeepIf [SelectCondition] | DropIf [SelectCondition]
    deriving (Eq, Show)

_parseConditions :: KwArgsValues -> NGLessIO MatchCondition
_parseConditions args = do
        keep_if <- lookupSymbolListOrScriptErrorDef (return []) "arguments to select" "keep_if" args
        drop_if <- lookupSymbolListOrScriptErrorDef (return []) "arguments to select" "drop_if" args
        keep_if' <- mapM asSC keep_if
        drop_if' <- mapM asSC drop_if
        case (keep_if', drop_if') of
            (cs, []) -> return $! KeepIf cs
            ([], cs) -> return $! DropIf cs
            (_, _) -> throwScriptError "To select, you cannot use both keep_if and drop_if"
    where
        asSC :: T.Text -> NGLessIO SelectCondition
        asSC "mapped" = return SelectMapped
        asSC "unmapped" = return SelectUnmapped
        asSC "unique" = return SelectUnique
        asSC c = throwShouldNotOccur ("Check failed.  Should not have seen this condition: '" ++ show c ++ "'")

-- Notes on "Sequence reinjection"
--
-- Here is the case where it is necessary:
--  1) the aligner includes the sequence in SamLine 1 (typically, the best hit, by its standards)
--  2) we filter out SamLine 1, but keep SamLine 2 (this is a rare case, a few in a million)
--  3) SamLine 2 does not have sequence information, so we need to reinject it
--
-- Here is why we need to rewrite the CIGAR string:
--  1) there are two forms of trimming in CIGAR: hard & soft. Hard means that
--  the bases were removed from the reads, whilst soft means that they are
--  kept. If one of the lines uses hard trimming and the other one uses soft
--  trimming, the read length may not match correctly
--  2) we want to keep the full sequence, so we want to use soft trimming (if
--  at all possible)
matchConditions :: Bool -> MatchCondition -> [(SamLine,B.ByteString)] -> NGLess [(SamLine, B.ByteString)]
matchConditions doReinject conds sg = reinjectSequences doReinject (matchConditions' conds sg)
    where
        reinjectSequences True f@((s@SamLine{}, _):rs)
            | not (any (hasSequence . fst) f) && any (hasSequence . fst) sg
                = do
                    s' <- addSequence s
                    return ((s', toStrictBS $ encodeSamLine s'):rs)
        reinjectSequences _ f = return f

        toStrictBS :: BB.Builder -> B.ByteString
        toStrictBS = BL.toStrict . BB.toLazyByteString

        addSequence s = case find hasSequence (fst <$> sg) of
                            Just s'@SamLine{} -> do
                                        cigar' <- fixCigar (samCigar s) (B.length $ samSeq s')
                                        return s { samSeq = samSeq s', samQual = samQual s', samCigar = cigar' }
                            _ -> return s

-- See note above on "Sequence reinjection" about why this function is necessary
fixCigar :: B.ByteString -> Int -> NGLess B.ByteString
fixCigar prev n = do
    prevM <- matchSize' True True prev
    if prevM == n
        then return prev
        else do
            let prev' = B8.map (\c -> if c == 'H' then 'S' else c) prev
            prevM' <- matchSize' True True prev'
            if prevM' == n
                then return prev'
                else throwDataError ("Cannot fix CIGAR string \"" ++ B8.unpack prev ++ "\" to represent a sequence of length " ++ show n)

matchConditions' :: MatchCondition -> [(SamLine,B.ByteString)] -> [(SamLine, B.ByteString)]
matchConditions' _ r@[(SamHeader _,_)] = r
matchConditions' (DropIf []) slines = slines
matchConditions' (DropIf (c:cs)) slines = matchConditions' (DropIf cs) (drop1 c slines)

matchConditions' (KeepIf []) slines = slines
matchConditions' (KeepIf (c:cs)) slines = matchConditions' (KeepIf cs) (keep1 c slines)

drop1 SelectUnmapped g = filter (isAligned . fst) g
drop1 SelectMapped g = if any (isAligned . fst) g
                            then []
                            else g
drop1 SelectUnique g = if isGroupUnique (map fst g) then [] else g

keep1 SelectMapped g = filter (isAligned . fst) g
keep1 SelectUnmapped g = if any (isAligned . fst) g
                            then []
                            else g
keep1 SelectUnique g = if isGroupUnique (map fst g) then g else []

-- readSamGroupsAsConduit :: (MonadIO m, MonadResource m) => FileOrStream -> C.Producer m [(SamLine, B.ByteString)]
-- The reason we cannot just use readSamGroupsC is that we want to get the unparsed ByteString on the side
readSamGroupsAsConduit istream paired =
        istream
            .| CC.concat
            .| readSamLineOrDie
            .| CL.groupBy groupLine
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
    Just lno <- ngleLno <$> nglEnvironment
    doReinject <- do
        v <- ngleVersion <$> nglEnvironment
        if v < NGLVersion 0 8
            then do
                outputListLno' WarningOutput ["Select changed behaviour (for the better) in ngless 0.8. If possible, upgrade your version statement."]
                return False
            else return True
    let (fpsam, istream') = asSamStream istream
        stream =
            readSamGroupsAsConduit istream' paired
                .| CL.mapM (runNGLess . matchConditions doReinject conditions)
                .| streamedSamStats lno ("select_"++T.unpack name) ("select.lno"++show lno)
                .| CL.map (V.fromList . map (ByteLine . snd))

        out = Stream [istream] ("selected_" ++ takeBaseNameNoExtensions fpsam ++ ".sam") stream
    return $! NGOMappedReadSet name out ref
executeSelect o _ = throwShouldNotOccur ("NGLESS type checking error (Select received " ++ show o ++ ")")

streamedSamStats lno ifile ref = C.passthroughSink (CL.map (V.singleton . map fst) .| samStatsC') $ \(total, aligned, unique) ->
    outputMappedSetStatistics (MappingInfo lno ifile ref total aligned unique)


splitSamlines3 = reverse3 . foldl' add1 ([],[],[])
    where
        reverse3 (g1, g2, gs) = (reverse g1, reverse g2, reverse gs)
        add1 (g1,g2,gs) s
            | isFirstInPair s = (s:g1,g2,gs)
            | isSecondInPair s = (g1, s:g2, gs)
            | otherwise = (g1, g2, s:gs)

data FilterAction = FADrop | FAUnmatch
    deriving (Eq)

data SelectGroupOptions = SelectGroupOptions
    !Double -- ^ min id
    !Int -- ^ minMatchSize
    !Int -- ^ maxTrim
    !Bool -- ^ reverse
    !FilterAction

applySelect :: Bool -> SelectGroupOptions -> SamGroup -> SamGroup
applySelect useNewer (SelectGroupOptions minID minMatchSize (-1) rev act) =
                    case act of
                        FADrop -> filter passTest
                        FAUnmatch -> map (unmatchUnless passTest)
    where
        okID
            | minID < 0.0 = const True
            | otherwise = \s -> fromRight 0.0 (matchIdentity useNewer s) >= minID
        okSize :: SamLine -> Bool
        okSize
            | minMatchSize == -1 = const True
            | otherwise = \s -> fromRight 0 (matchSize useNewer s) >= minMatchSize
        rawTest s = okID s && okSize s
        passTest
            | rev = not . rawTest
            | otherwise = rawTest
applySelect useNewer (SelectGroupOptions minID minMatch maxTrim rev act) = \samlines ->
                    let samlines' = applySelect useNewer (SelectGroupOptions minID minMatch (-1) rev act) samlines
                        (g1,g2,gs) = splitSamlines3 samlines'
                        (s1,s2,ss) = (seqSize g1, seqSize g2, seqSize gs)
                        okTrim :: Int -> SamLine -> Bool
                        okTrim seqlen = \s -> (seqlen - fromRight 0 (matchSize useNewer s) <= maxTrim)
                        in case act of
                            FADrop -> filter (okTrim s1) g1 ++ filter (okTrim s2) g2 ++ filter (okTrim ss) gs
                            FAUnmatch -> map (unmatchUnless $ okTrim s1) g1 ++ map (unmatchUnless $ okTrim s2) g2 ++ map (unmatchUnless $ okTrim ss) gs

unmatchUnless c s
    | not (c s) = unmatch s
    | otherwise = s
unmatch samline = samline { samFlag = (samFlag samline .|. 4) `clearBit` 1, samRName = "*", samCigar = "*" }
seqSize :: SamGroup -> Int
seqSize = maximum . map (B.length . samSeq)

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
    minMatchSize <- fromInteger <$> lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "min_match_size" kwargs
    maxTrim <- fromInteger <$> lookupIntegerOrScriptErrorDef (return (-1)) "filter method" "max_trim" kwargs
    reverseTest <- lookupBoolOrScriptErrorDef (return False) "filter method" "reverse" kwargs
    useNewer <- lookupBoolOrScriptErrorDef (return False) "filter method" "__version11_or_higher" kwargs
    action <- lookupSymbolOrScriptErrorDef (return "drop") "filter method" "action" kwargs >>= \case
        "drop" -> return FADrop
        "unmatch" -> return FAUnmatch
        other -> throwScriptError ("unknown action in filter(): `" ++ T.unpack other ++"`.\nAllowed values are:\n\tdrop\n\tunmatch\n\tkeep\n")
    let samlines' = applySelect useNewer (SelectGroupOptions (fromInteger minID / 100.0) minMatchSize maxTrim reverseTest action) samlines
    return (NGOMappedRead samlines')
executeMappedReadMethod (MethodName "unique") samlines Nothing [] = return . NGOMappedRead . mUnique $ samlines
executeMappedReadMethod (MethodName "allbest") samlines Nothing kwargs = do
    useNewer <- lookupBoolOrScriptErrorDef (return False) "filter method" "__version11_or_higher" kwargs
    return . NGOMappedRead . mBesthit useNewer $ samlines
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

mBesthit :: Bool -> [SamLine] -> [SamLine]
mBesthit _ [] = []
mBesthit _ sl@[_] = sl
mBesthit useNewer slines = let (g1,g2,gs) = splitSamlines3 slines
                    in mBesthit' useNewer g1 ++ mBesthit' useNewer g2 ++ mBesthit' useNewer gs

mBesthit' :: Bool -> [SamLine] -> [SamLine]
mBesthit' _ [] = []
mBesthit' _ sl@[_] = sl
mBesthit' useNewer samlines = case mapMaybe extract samlines of
        [] -> samlines
        extracted ->
            let
                ms = maximum (fst3 <$> extracted)
                -- fractional distance
                ds :: Ratio Int
                ds = minimum ((\(_, e, _) -> e % ms) <$> extracted)
            in mapMaybe (\(_, e, sl) ->
                        if e % ms <= ds
                            then Just sl
                            else Nothing) extracted

    where
        extract :: SamLine -> Maybe (Int, Int, SamLine)
        extract sl = do
            dist <- samIntTag sl "NM"
            size <- eitherToMaybe (matchSize useNewer sl)
            return (size, dist, sl)

isGroupUnique :: [SamLine] -> Bool
isGroupUnique [] = True
isGroupUnique [_] = True
isGroupUnique [f,s] = (isFirstInPair f /= isFirstInPair s) &&
                            (not (isAligned f && isAligned s) || (samRName f == samRName s))
isGroupUnique _ = False

