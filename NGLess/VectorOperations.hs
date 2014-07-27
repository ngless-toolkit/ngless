
module VectorOperations
 (
 zeroVec,
 incV,
 incVec,
 getV
 ) where

import qualified Data.Vector.Unboxed.Mutable as VM

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec

incV False _ _ = return ()
incV True  v i = incVec v i
   
incVec v i = VM.unsafeRead v i >>= \c -> VM.unsafeWrite v i (c + 1)

getV vec i =  V.unsafeIndex vec i

