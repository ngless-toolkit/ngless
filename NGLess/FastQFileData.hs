{-# LANGUAGE BangPatterns #-}
module FastQFileData
    (
        Result(..),  iterateFile, addToCount, countChars, seqMinMax
    ) where

import Control.DeepSeq
import Control.Monad    
import Control.Monad.ST

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Map
import Data.Char

data Result =  Result {bpCounts :: (Int, Int, Int, Int) , lc :: Char, qualCounts ::  [Map Char Int], nSeq :: Int, seqSize :: (Int,Int)} deriving(Show)

instance NFData Result where
    rnf (Result (!_,!_,!_,!_) !_ cs !_ (!_,!_)) = rnf cs


-- mapAddFunction :: To increase the old_value value by 1 of a given key.
mapAddFunction _ new_value old_value = old_value + new_value

-- addEachCount :: Update Map counts with a new quality.
addEachCount :: Map Char Int -> Char -> Map Char Int
addEachCount counts qual = insertWithKey mapAddFunction qual 1 counts

{--
addToCount :: [Map Char Int] -> BL.ByteString -> [Map Char Int]
addToCount counts qual = runST $ do
    let sQual = BL.toStrict qual
        sSize = B.length sQual
        cLength = length counts
    res <- VM.new sSize
    forM_ [0..sSize - 1] $ \i -> do
        if i >= cLength
            then VM.write res i $ addEachCount (fromList []) (B.index sQual i)
            else VM.write res i $ addEachCount (counts !! i) (B.index sQual i)
    res' <- V.unsafeFreeze res
    return $ V.toList res'
--}
  
addToCount :: [Map Char Int] -> String -> [Map Char Int]
addToCount counts qual = addToCount' counts qual
        where
            addToCount' (c:xs) (q:ys) = (addEachCount c q) : addToCount' xs ys
            addToCount' [] (q:ys) = (addEachCount (fromList []) q) : addToCount' [] ys 
            addToCount' c [] = c

wc :: BL.ByteString -> V.Vector Int
wc st = runST $ do
    counts <- VM.new 256 -- number max of chars
    forM_ [0..255] $ \i -> do
        VM.unsafeWrite counts i 0
    forM_ (BL.toChunks st) $ \c -> do
        forM_ [0..B.length c - 1] $ \i -> do
            let w = ord . toUpper $ B.index c i
            cur <- VM.unsafeRead counts w
            VM.unsafeWrite counts w (1 + cur)
    res <- V.unsafeFreeze counts
    return res

countChars :: (Int,Int,Int,Int) -> BL.ByteString -> (Int,Int,Int,Int)
countChars (a,b,c,d) s = do 
    let res = wc s
    (a + getCount res 'A', b + getCount res 'C', c + getCount res 'G', d + getCount res 'T') 
    where getCount res pos = (res V.! (ord pos)) 


seqMinMax :: (Int,Int) -> Int -> (Int,Int)
seqMinMax (minSeq, maxSeq) length' = ((min length' minSeq),(max length' maxSeq))

--updateResults :: Used to fill in the structure "Result" with the FastQ file info.
updateResults :: Result -> BL.ByteString -> BL.ByteString -> Result
updateResults fileData seq' qual = Result (countChars (bpCounts fileData) seq')
                                         (BL.foldr min (lc fileData) qual)
                                         (addToCount (qualCounts fileData) (BL.unpack qual))
                                         ((nSeq fileData) + 1)
                                         (seqMinMax (seqSize fileData) (fromIntegral (BL.length seq')))

--iterateFile :: Used to iterate the file in a strict manner.
iterateFile :: BL.ByteString -> Result
iterateFile contents = iterateFile' initial (BL.lines contents)
        where
                initial = Result (0,0,0,0) '~' [] 0 (maxBound :: Int, minBound :: Int)
                iterateFile' r (_:seq':_:quals:xs) = r `deepseq`
                        iterateFile' (updateResults r seq' quals) xs
                iterateFile' !r [] = r `deepseq` r
                iterateFile' _ _  = error "Number of lines is not multiple of 4!"
