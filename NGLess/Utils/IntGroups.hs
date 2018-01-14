{- Copyright 2016-2018 NGLess Authors
 - License: MIT -}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.IntGroups
    ( IntGroups(..)

    , fromList
    , toList

    , empty
    , length
    , null

    , forM_
    ) where
import qualified Data.Vector.Unboxed as VU
import qualified Control.Monad
import Control.DeepSeq          (NFData(..))
import qualified Prelude
import Prelude hiding (length, null)

-- | IntGroups are a faster, strict, representation of [[Int]]
data IntGroups = IntGroups !(VU.Vector Int) !(VU.Vector Int)
instance NFData IntGroups where
    rnf (IntGroups !_ !_) = ()


fromList :: [[Int]] -> IntGroups
fromList gs = IntGroups values indices
    where
        values = VU.fromList (concat gs)
        indices = VU.fromList (scanl (+) 0 $ map Prelude.length gs)

toList :: IntGroups -> [VU.Vector Int]
toList (IntGroups values indices) = go 0
    where
        go ix
            | ix == VU.length indices - 1 = []
            | otherwise = VU.slice st n values:go (ix+1)
                where
                    st = indices VU.! ix
                    e = indices VU.! (ix + 1)
                    n = e - st
{-# INLINE toList #-}

length (IntGroups _ indices) = VU.length indices - 1
{-# INLINE length #-}

null (IntGroups values _) = VU.null values
{-# INLINE null #-}

empty = IntGroups VU.empty VU.empty

forM_ :: (Monad m) => IntGroups -> ((VU.Vector Int) -> m ()) -> m ()
forM_ vs = Control.Monad.forM_ (toList vs)
{-# INLINE forM_ #-}

