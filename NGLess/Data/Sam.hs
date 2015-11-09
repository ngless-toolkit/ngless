module Data.Sam
    ( SamLine(..)
    , SamResult(..)
    , samLength
    , readAlignments
    , readSamLine
    , isAligned
    , isUnique
    , isPositive
    , isNegative
    , hasQual
    , matchIdentity
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as S8
import Control.DeepSeq

import Control.Applicative
import Data.Bits (testBit)
import NGLess.NGError


data SamLine = SamLine
            { samQName :: !B.ByteString
            , samFlag :: !Int
            , samRName :: !B.ByteString
            , samPos :: !Int
            , samMapq :: !Int
            , samCigar :: !B.ByteString
            , samRNext :: !B.ByteString
            , samPNext :: !Int
            , samTLen :: !Int
            , samSeq :: !B.ByteString
            , samQual :: !B.ByteString
            } | SamHeader !B.ByteString
             deriving (Eq, Show, Ord)


instance NFData SamLine where
    rnf (SamLine !qn !f !r !p !m !c !rn !pn !tl !s !qual) = ()
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

isUnique :: SamLine -> Bool
isUnique =  (> 0) . samMapq

-- 512 -> 8
hasQual :: SamLine -> Bool
hasQual = (`testBit` 9) . samFlag


readAlignments :: BL.ByteString -> [SamLine]
readAlignments = readAlignments' . L8.lines

readAlignments' :: [BL.ByteString] -> [SamLine]
readAlignments' [] = []
readAlignments' (l:ls)
    | L8.head l == '@' = readAlignments' ls
    | otherwise = readSamLine' l:readAlignments' ls


readInt b = case L8.readInt b of
    Just (v,_) -> Right v
    _ -> Left ("Expected int, got " ++ show b)

readSamLine' :: BL.ByteString -> SamLine
readSamLine' = rightOrError . readSamLine
    where
        rightOrError (Right v) = v
        rightOrError (Left err) = error err

readSamLine :: BL.ByteString -> Either String SamLine
readSamLine line
    | BL8.head line == '@' = return (SamHeader $ strict line)
    | otherwise = case BL8.split '\t' line of
    (tk0:tk1:tk2:tk3:tk4:tk5:tk6:tk7:tk8:tk9:tk10:_) ->
        SamLine
            <$> pure (strict tk0)
            <*> readInt tk1
            <*> pure (strict tk2)
            <*> readInt tk3
            <*> readInt tk4
            <*> pure (strict tk5)
            <*> pure (strict tk6)
            <*> readInt tk7
            <*> readInt tk8
            <*> pure (strict tk9)
            <*> pure (strict tk10)
    tokens -> Left $ concat ["Expected 11 tokens, only got ", show $ length tokens,"\n\t\tLine was '", show line, "'"]

strict :: BL.ByteString -> B.ByteString
strict = B.concat . BL.toChunks


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
                n' = if code `elem` "DNSHX" then n else 0
            r <- numberMismatches rest
            return (n' + r)

matchIdentity :: SamLine -> Either NGError Double
matchIdentity samline = do
        errors <- numberMismatches (samCigar samline)
        let len = samLength samline
            toDouble = fromInteger . toInteger
            mid = toDouble (len - errors) / toDouble len
        return mid
