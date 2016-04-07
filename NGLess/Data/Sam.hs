{- Copyright 2014-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Sam
    ( SamLine(..)
    , SamResult(..)
    , samLength
    , readSamGroupsC
    , readSamLine
    , isAligned
    , isPositive
    , isNegative
    , hasQual
    , matchIdentity
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Strict.Tuple (Pair(..))
import Data.Bits (testBit)
import Control.DeepSeq

import Data.Conduit ((=$=))
import Data.Function (on)
import Control.Monad.Except
import NGLess.NGError
import Utils.Conduit


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
    rnf (SamLine !qn !f !r !p !m !c !rn !pn !tl !s !qual !extra) = ()
    rnf (SamHeader !_) = ()


data SamResult = Total | Aligned | Unique | LowQual deriving (Enum)

samLength = B8.length . samSeq

-- log 2 of N
-- 4 -> 2
isAligned :: SamLine -> Bool
isAligned = not . (`testBit` 2) . samFlag

-- 16 -> 4
isNegative :: SamLine -> Bool
isNegative = (`testBit` 4) . samFlag

-- all others
isPositive :: SamLine -> Bool
isPositive = not . isNegative

-- 512 -> 8
hasQual :: SamLine -> Bool
hasQual = (`testBit` 9) . samFlag


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

readSamLine :: B.ByteString -> Either NGError SamLine
readSamLine line
    | B8.head line == '@' = return (SamHeader line)
    | otherwise = case runSimpleParser samP line of
        Just (v :!: _) -> return v
        Nothing -> throwDataError ("Could not parse sam line "++show line)

tabDelim :: SimpleParser B.ByteString
tabDelim = SimpleParser $ \input -> do
    ix <- B8.elemIndex '\t' input
    return $! (B.take ix input :!: B.drop (ix+1) input)

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
    <*> tabDelim
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

numberMismatches :: B.ByteString -> Either NGError Int
numberMismatches cigar
    | B8.null cigar = return 0
    | otherwise = case B8.readInt cigar of
        Nothing -> throwDataError ("could not parse cigar '"++S8.unpack cigar ++"'")
        Just (n,code_rest) -> do
            let code = S8.head code_rest
                rest = S8.tail code_rest
                n' = if code `elem` ("DNSHX" :: [Char]) then n else 0
            r <- numberMismatches rest
            return (n' + r)

matchIdentity :: SamLine -> Either NGError Double
matchIdentity samline = do
        errors <- numberMismatches (samCigar samline)
        let len = samLength samline
            toDouble = fromInteger . toInteger
            mid = toDouble (len - errors) / toDouble len
        return mid

-- | take in *lines* and transform them into groups of SamLines all refering to the same read
readSamGroupsC :: (MonadError NGError m) => C.Conduit ByteLine m [SamLine]
readSamGroupsC = readSamLineOrDie =$= CL.groupBy groupLine
    where
        readSamLineOrDie = C.awaitForever $ \(ByteLine line) ->
            case readSamLine line of
                Left err -> throwError err
                Right parsed@SamLine{} -> C.yield parsed
                _ -> return ()
        groupLine = (==) `on` samQName

