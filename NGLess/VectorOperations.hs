
module VectorOperations
    (
 zeroVec,
 incV,
 incVec
    ) where

import qualified Data.Vector.Unboxed.Mutable as VM

zeroVec n = do
    vec <- VM.unsafeNew n
    VM.set vec (0 :: Int)
    return vec


incV False _ _ = return ()
incV True v i = incVec v i
   
incVec v i = do
    cur <- VM.unsafeRead v i
    VM.unsafeWrite v i (cur + 1)
