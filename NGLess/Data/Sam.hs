{- Copyright 2014-2019 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Data.Sam
    ( SamLine(..)
    , SamGroup
    , samLength
    , readSamGroupsC'
    , readSamLine
    , encodeSamLine
    , isAligned
    , isPositive
    , isNegative
    , isFirstInPair
    , isSecondInPair
    , isSamHeaderString
    , hasSequence
    , matchSize
    , matchSize'
    , matchIdentity

    , samStatsC
    , samStatsC'
    , samIntTag
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lift as C
import qualified Data.Conduit as C
import           Data.Conduit.Algorithms.Async as CAlg
import           Data.Conduit.Algorithms.Utils (awaitJust)
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit ((.|))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Strict.Tuple (Pair(..))
import Data.Bits (testBit)
import Data.List (intersperse)
import Control.Error (note)
import Control.DeepSeq

import Data.Maybe
import Control.Monad.Except
import NGLess.NGError
import Utils.Utils
import Utils.Conduit (ByteLine(..))


type SamGroup = [SamLine]

data SamLine = SamLine
            { samQName :: !B.ByteString
            , samFlag :: {-# UNPACK #-} !Int
            , samRName :: {-# UNPACK #-} !B.ByteString
            , samPos :: {-# UNPACK #-} !Int
            , samMapq :: {-# UNPACK #-} !Int
            , samCigar :: {-# UNPACK #-} !B.ByteString
            , samRNext :: {-# UNPACK #-} !B.ByteString
            , samPNext :: {-# UNPACK #-} !Int
            , samTLen :: {-# UNPACK #-} !Int
            , samSeq :: {-# UNPACK #-} !B.ByteString
            , samQual :: {-# UNPACK #-} !B.ByteString
            , samExtra :: {-# UNPACK #-} !B.ByteString
            } | SamHeader !B.ByteString
             deriving (Eq, Show, Ord)


instance NFData SamLine where
    rnf SamLine{} = ()
    rnf (SamHeader !_) = ()

isHeader SamHeader{} = True
isHeader SamLine{} = False
{-# INLINE isHeader #-}

samLength :: SamLine -> Int
samLength = B8.length . samSeq
{-# INLINE samLength #-}

-- log 2 of N
-- 4 -> 2
isAligned :: SamLine -> Bool
isAligned = not . (`testBit` 2) . samFlag
{-# INLINE isAligned #-}

-- 16 -> 4
isNegative :: SamLine -> Bool
isNegative = (`testBit` 4) . samFlag
{-# INLINE isNegative #-}

-- all others
isPositive :: SamLine -> Bool
isPositive = not . isNegative
{-# INLINE isPositive #-}

isFirstInPair :: SamLine -> Bool
isFirstInPair = (`testBit` 6) . samFlag
{-# INLINE isFirstInPair #-}

isSecondInPair :: SamLine -> Bool
isSecondInPair = (`testBit` 7) . samFlag
{-# INLINE isSecondInPair #-}

isSamHeaderString :: B.ByteString -> Bool
isSamHeaderString s = not (B.null s) && (B.head s == 64) -- 64 is '@'
{-# INLINE isSamHeaderString #-}

hasSequence :: SamLine -> Bool
hasSequence SamHeader{} = False
hasSequence SamLine{samSeq=s} = s /= "*"
{-# INLINE hasSequence #-}

newtype SimpleParser a = SimpleParser { runSimpleParser :: B.ByteString -> Maybe (Pair a B.ByteString) }
instance Functor SimpleParser where
    fmap f p = SimpleParser $ \b -> do
                                (v :!: rest) <- runSimpleParser p b
                                return $! (f v :!: rest)

instance Applicative SimpleParser where
    pure v = SimpleParser (\b -> Just (v :!: b))
    f <*> g = SimpleParser (\b -> do
                                (f' :!: rest) <- runSimpleParser f b
                                (g' :!: rest') <- runSimpleParser g rest
                                return $! (f' g' :!: rest'))

encodeSamLine :: SamLine -> BB.Builder
encodeSamLine (SamHeader b) = BB.byteString b
encodeSamLine samline = mconcat . intersperse (BB.char7 '\t') $ map ($ samline)
    [ BB.byteString . samQName
    , BB.intDec     . samFlag
    , BB.byteString . samRName
    , BB.intDec     . samPos
    , BB.intDec     . samMapq
    , BB.byteString . samCigar
    , BB.byteString . samRNext
    , BB.intDec .     samPNext
    , BB.intDec     . samTLen
    , BB.byteString . samSeq
    , BB.byteString . samQual
    , BB.byteString . samExtra
    ]

readSamLine :: B.ByteString -> Either NGError SamLine
readSamLine line
    | B.null line = throwDataError "Unexpected empty line"
    | B8.head line == '@' = return (SamHeader line)
    | otherwise = case runSimpleParser samP line of
        Just (v :!: _) -> return v
        Nothing -> throwDataError ("Could not parse sam line "++show line)

tabDelim :: SimpleParser B.ByteString
tabDelim = SimpleParser $ \input -> do
    ix <- B8.elemIndex '\t' input
    return $! (B.take ix input :!: B.drop (ix+1) input)

tabDelimOpts :: SimpleParser B.ByteString
tabDelimOpts = SimpleParser $ \input ->
    case B8.elemIndex '\t' input of
         Just ix -> return $! (B.take ix input :!: B.drop (ix+1) input)
         Nothing -> return $! (B.empty :!: input)

readIntTab = SimpleParser $ \b -> do
        (v,rest) <- B8.readInt b
        return $! (v :!: B.tail rest)
restParser = SimpleParser $ \b -> Just (b :!: B.empty)
samP = SamLine
    <$> tabDelim
    <*> readIntTab
    <*> tabDelim
    <*> readIntTab
    <*> readIntTab
    <*> tabDelim
    <*> tabDelim
    <*> readIntTab
    <*> readIntTab
    <*> tabDelim
    <*> tabDelimOpts
    <*> restParser

{--
Op     Description
M alignment match (can be a sequence match or mismatch).
I insertion to the reference
D deletion from the reference.
N skipped region from the reference.
S soft clipping (clipped sequences present inSEQ)
H hard clipping (clipped sequences NOT present inSEQ)
P padding (silent deletion from padded reference).
= sequence match.
X sequence mismatch.
--}

matchSize :: Bool -> SamLine -> Either NGError Int
matchSize includeI = matchSize' includeI False . samCigar

matchSize' includeI includeSoft cigar
    | B8.null cigar = return 0
    | otherwise = case B8.readInt cigar of
        Nothing -> throwDataError ("could not parse cigar '"++B8.unpack cigar ++"'")
        Just (n,code_rest) -> do
            let code = B8.head code_rest
                rest = B8.tail code_rest
                n' = case code of
                            'M' -> n
                            '=' -> n
                            'X' -> n
                            'S'
                                | includeSoft -> n
                            'I'
                                | includeI -> n
                            _ -> 0
            r <- matchSize' includeI includeSoft rest
            return (n' + r)

matchIdentity :: Bool -> SamLine -> Either NGError Double
matchIdentity useNewer samline = do
    let errmsg = "Could not get NM tag for samline " ++ B8.unpack (samQName samline) ++ ", extra tags were: "++ B8.unpack (samExtra samline)
    errors <- note (NGError DataError errmsg) $ samIntTag samline "NM"
    len <- matchSize useNewer samline
    let toDouble = fromInteger . toInteger
        mid = toDouble (len - errors) / toDouble len
    return mid

samIntTag :: SamLine -> B.ByteString -> Maybe Int
samIntTag samline tname
    | isHeader samline = Nothing
    | otherwise = listToMaybe . mapMaybe gettag . B8.split '\t' . samExtra $ samline
    where
        gettag match
            | B.take 2 match == tname
                    && (fst <$> B8.uncons (B.drop 3 match)) == Just 'i' = fst <$> B8.readInt (B.drop 5 match)
            | otherwise = Nothing

-- | take in *ByteLines* and transform them into groups of SamLines all
-- refering to the same read
--
-- This works in chunks (vectors) for efficiency
--
-- Header lines are silently ignored
--
-- When respectPairs is False, then the two mates of the same fragment will be
-- considered grouped in different blocks
readSamGroupsC' :: forall m . (MonadError NGError m, MonadIO m) =>
            Int -- ^ number of threads
            -> Bool -- respectPairs
            -> C.ConduitT (V.Vector ByteLine) (V.Vector [SamLine]) m ()
readSamGroupsC' mapthreads respectPairs = do
        CC.dropWhileE (isSamHeaderString . unwrapByteLine)
        CC.filter (not . V.null)
            .| asyncMapEitherC mapthreads (fmap groupByName . V.mapM (readSamLineCheck . unwrapByteLine))
            -- the groups may not be aligned on the group boundary, thus we need to fix them
            .| fixSamGroups
    where
        samQNameMate :: SamLine -> (B.ByteString, Bool)
        samQNameMate s
            | respectPairs = (samQName s, True)
            | otherwise = (samQName s, isFirstInPair s)
        readSamLineCheck sm
            | isSamHeaderString sm = throwDataError "SAM header appears after SAM data (malformed file?)"
            | otherwise = readSamLine sm
        groupByName :: V.Vector SamLine -> V.Vector [SamLine]
        groupByName vs = V.unfoldr groupByName' (0, error "null value", [])
            where
                groupByName' :: (Int, (B.ByteString, Bool), [SamLine]) -> Maybe ([SamLine], (Int, (B.ByteString, Bool), [SamLine]))
                groupByName' (ix, name, acc)
                        | ix == V.length vs = if null acc
                                                then Nothing
                                                else Just (reverse acc, (ix, error "null value", []))
                        | null acc = groupByName' (ix + 1, cur_tag, [cur])
                        | cur_tag == name = groupByName' (ix + 1, name, cur:acc)
                        | otherwise = Just (reverse acc, (ix, error "null value", []))
                    where
                        cur = vs V.! ix
                        cur_tag = samQNameMate cur
        fixSamGroups :: C.ConduitT (V.Vector [SamLine]) (V.Vector [SamLine]) m ()
        fixSamGroups = awaitJust fixSamGroups'
        fixSamGroups' :: V.Vector [SamLine] -> C.ConduitT (V.Vector [SamLine]) (V.Vector [SamLine]) m ()
        fixSamGroups' prev = C.await >>= \case
            Nothing -> C.yield prev
            Just cur -> case (V.last prev, V.head cur) of
                    (lastprev:_, curfirst:_)
                        | samQNameMate lastprev /= samQNameMate curfirst -> do
                            -- lucky case, the groups align with the vector boundary
                            C.yield prev
                            fixSamGroups' cur
                        | otherwise -> do
                            C.yield (V.slice 0 (V.length prev - 1) prev)
                            cur' <- liftIO $ injectLast (V.last prev) cur
                            fixSamGroups' cur'
                    _ -> throwShouldNotOccur "This should have been an impossible situation in `fixSamGroups`"
        injectLast :: [SamLine] -> V.Vector [SamLine] -> IO (V.Vector [SamLine])
        injectLast prev gs = do
            gs' <- V.unsafeThaw gs
            VM.modify gs' (prev ++ ) 0
            V.unsafeFreeze gs'

samStatsC :: (MonadIO m) => C.ConduitT (V.Vector ByteLine) C.Void m (NGLess (Int, Int, Int))
samStatsC = C.runExceptC $ readSamGroupsC' 1 True .| samStatsC'

samStatsC' :: forall m. (MonadError NGError m) => C.ConduitT (V.Vector SamGroup) C.Void m (Int, Int, Int)
samStatsC' = CL.foldM summarizeV (0, 0, 0)
    where
        add1if !v True = v+1
        add1if !v False = v

        summarizeV :: (Int, Int, Int) -> V.Vector SamGroup -> m (Int, Int, Int)
        summarizeV c vs = V.foldM summarize c vs

        summarize :: (Int, Int, Int) -> SamGroup -> m (Int, Int, Int)
        summarize c [] = return c
        summarize c (samline:_)
            | isHeader samline = return c
        summarize (!t, !al, !u) g = let
                    aligned = any isAligned g
                    sameRName = allSame (samRName <$> g)
                    unique = aligned && sameRName
            in return
                (t + 1
                ,add1if al aligned
                ,add1if  u unique
                )
