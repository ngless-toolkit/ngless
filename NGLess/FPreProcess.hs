module FPreProcess
    (

    ) where

--TODO: receive a read of type (id:seq:qual:_:xs) and use the quality to calculateSubStrim'
--substrim :: String -> Int -> (Int,Int)

calculateSubStrim qual cutoff = calculateSubStrim' qual 0 0 0 (0,0)
    where   calculateSubStrim' [] _ _ _ (index,size) = (index,size)
            calculateSubStrim' (q:xs) inc offset act_index (index, size) =
                (if q >= cutoff
                    then calculateSubStrim' xs (inc + 1) (offset + 1) act_index (if (inc + 1) > size
                                                                         then (act_index , (1 + size))
                                                                         else (index, size))
                    else calculateSubStrim' xs 0 (offset + 1) (offset + 1) (index,size))
