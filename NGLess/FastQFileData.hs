{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FastQFileData
    (
        Result(..), initVec, iterateFile, addToCount, countChars, seqMinMax, addEachCount
    ) where

import Control.DeepSeq
import Control.Monad    
import Control.Monad.ST

import Control.Monad.Par

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Data.Char

data Result =  Result {bpCounts :: (Int, Int, Int, Int) , lc :: Char, qualCounts ::  [V.Vector Int], nSeq :: Int, seqSize :: (Int,Int)} deriving(Show)

instance NFData Result where
    rnf (Result (!_,!_,!_,!_) !_ cs !_ (!_,!_)) = rnf cs

{-
    Auxiliary functions to work with Mutable Vectors.
-}

initVec :: V.Vector Int
initVec = runST $ do
    counts <- VM.unsafeNew 256 -- number max of chars
    forM_ [0..255] $ \i -> do
        VM.unsafeWrite counts i 0
    res <- V.unsafeFreeze counts
    return res        

wc :: BL.ByteString -> V.Vector Int
wc st = runST $ do
    counts <- VM.unsafeNew 256 -- number max of chars
    forM_ [0..255] $ \i -> do
        VM.unsafeWrite counts i 0
    forM_ (BL.toChunks st) $ \c -> do
        forM_ [0..B.length c - 1] $ \i -> do
            let w = ord . toUpper $ B.index c i -- 'a' -> 'A', etc.
            cur <- VM.unsafeRead counts w
            VM.unsafeWrite counts w (1 + cur)
    res <- V.unsafeFreeze counts
    return res



-- addEachCount :: Update Map counts with a new quality.
addEachCount :: V.Vector Int -> Char -> V.Vector Int
addEachCount counts qual = do
    let i = ord qual
        r = V.unsafeIndex counts i
    V.modify (\v -> VM.write v i (r + 1)) counts 

 
addToCount :: [V.Vector Int] -> BL.ByteString -> [V.Vector Int]
addToCount counts qual = addToCount' counts qual
        where
            addToCount' c "" = c -- c size > q bps
            addToCount' [] q = addEachCount (initVec) ( BL.head q ) : addToCount' [] (BL.tail q)  -- c size < q bps
            addToCount' (c:xs) q = addEachCount c (BL.head q) : addToCount' xs (BL.tail q) -- normal case 1 to 1
    


countChars :: (Int,Int,Int,Int) -> BL.ByteString -> (Int,Int,Int,Int)
countChars (a,b,c,d) s = do 
    let res = wc s
    (a + getCount res 'A', b + getCount res 'C', c + getCount res 'G', d + getCount res 'T') 
    where getCount res pos = (V.unsafeIndex res (ord pos)) 


seqMinMax :: (Int,Int) -> Int -> (Int,Int)
seqMinMax (minSeq, maxSeq) length' = ((min length' minSeq),(max length' maxSeq))

--updateResults :: Used to fill in the structure "Result" with the FastQ file info.
updateResults :: Result -> BL.ByteString -> BL.ByteString -> Result
updateResults fileData seq' qual = Result (countChars (bpCounts fileData) seq')
                                         (BL.foldr min (lc fileData) qual)
                                         (addToCount (qualCounts fileData) qual)
                                         ((nSeq fileData) + 1)
                                         (seqMinMax (seqSize fileData) (fromIntegral (BL.length seq')))

--iterateFile :: Used to iterate the file in a strict manner.
iterateFile :: BL.ByteString -> Result
iterateFile contents = iterateFile' initial (BL.lines contents)
        where
                initial = Result (0,0,0,0) '~' [] 0 (maxBound :: Int, minBound :: Int)
                iterateFile' r (_:seq':_:quals:xs) = r `deepseq`
                        iterateFile' (updateResults r seq' quals) xs
                iterateFile' r [] = r
                iterateFile' _ _  = error "Number of lines is not multiple of 4!"
