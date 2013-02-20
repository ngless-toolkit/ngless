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
import qualified Data.Text as T
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec

-- main function of this module
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict Text) before parsing
parsengless :: T.Text -> T.Text -> Either T.Text Expression
parsengless _inputname input = case  parse nglparser "input" input of
            Right val -> Right val
            Left err -> Left (T.pack . show $ err)

nglparser :: Parser Expression
nglparser = many1 expression >>= return . Sequence
expression = (try symbol) <|>
            (try nglstr) <|>
            (try funccall)


symbol = (char ':')  *> (liftA ConstSymbol $ ngltoken) <* (char ':')
nglstr = (char '"') *> (liftA (ConstStr . T.pack) $ many (noneOf "\"")) <* (char '"')

ngltoken :: Parser T.Text
ngltoken = T.pack <$> many1 (oneOf asciiLetters)

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


functionOf :: T.Text -> Either T.Text FuncName
functionOf "fastq" = Right Ffastq
functionOf "substrim" = Right Fsubstrim
functionOf "map" = Right Fmap
functionOf "count" = Right Fcount
functionOf "write" = Right Fwrite
functionOf "print" = Right Fprint
functionOf _ = Left "Function not found"

kwarg = pure (,) <*> (Variable <$> ngltoken <* char '=') <*> expression

asciiLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
