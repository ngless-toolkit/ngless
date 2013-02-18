{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}

module Parse
    ( parsengless
    ) where

import Language

import Data.Maybe
import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.Parsec

-- main function of this module
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict ByteString) before parsing
parsengless :: S.ByteString -> S.ByteString -> Either S.ByteString Expression
parsengless _inputname input = case  parse nglparser "input" input of
            Right val -> Right val
            Left err -> Left (S8.pack . show $ err)

nglparser :: Parser Expression
nglparser = many1 expression >>= return . Sequence
expression = symbol
symbol = (char ':')  *> (liftA ConstSymbol $ nglstring) <* (char ':')
nglstring = liftA S8.pack $ many1 (oneOf asciiLetters)

asciiLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
