{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parsengless
    ) where

import Language

import Data.Maybe
import Control.Applicative hiding ((<|>), many)
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
expression = (try symbol) <|>
            (try nglstr) <|>
            (try funccall)


symbol = (char ':')  *> (liftA ConstSymbol $ ngltoken) <* (char ':')
nglstr = (char '"') *> (liftA (ConstStr . S8.pack) $ many (noneOf "\"")) <* (char '"')

ngltoken :: Parser S.ByteString
ngltoken = liftA S8.pack $ many1 (oneOf asciiLetters)

funccall :: Parser Expression
funccall = do
    fname <- ngltoken
    _ <- char '('
    arg <- expression
    _ <- char ','
    kwargs <- many (kwarg <* (char ','))
    _ <- char ')'
    case functionOf fname of
        Right f -> return (FunctionCall f [arg] kwargs Nothing)
        Left err -> error (show err)


functionOf :: S.ByteString -> Either S.ByteString FuncName
functionOf "fastq" = Right Ffastq
functionOf _ = Left "Function not found"

kwarg = pure (,) <*> (Variable <$> ngltoken <* char '=') <*> expression

asciiLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
