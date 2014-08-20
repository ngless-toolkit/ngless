module VectorOperations
 ( zeroVec
 , unsafeIncrement
 ) where

import qualified Data.Vector.Unboxed.Mutable as VM


zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec

unsafeIncrement v i = do
    c <- VM.unsafeRead v i
    VM.unsafeWrite v i (c + 1)

