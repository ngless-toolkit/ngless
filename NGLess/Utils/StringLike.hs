{-# LANGUAGE FlexibleInstances #-}
module Utils.StringLike
    ( StringLike(..)
    ) where

import qualified Data.Text as T

class StringLike s where
    asString :: s -> String
    asText :: s -> T.Text

    asString = T.unpack . asText
    asText = T.pack . asString

    {-# MINIMAL asString | asText #-}



instance StringLike String where
    asString = id

instance StringLike T.Text where
    asText = id
