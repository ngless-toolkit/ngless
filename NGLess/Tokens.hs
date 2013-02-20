{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE OverloadedStrings #-}

module Tokens
    ( Token(..)
    , tokenize
    ) where

import qualified Data.Text as T
import Control.Applicative hiding ((<|>), many)
import Control.Monad (void)
import Text.Parsec.Text ()
import Text.Parsec.Combinator
import Text.Parsec
import Language

data Token =
        TExpr Expression
        | TWord T.Text
        | TReserved T.Text
        | TOperator Char
        | TNewLine
        | TComment Int
        | TIndent Int
    deriving (Eq, Show)

tokenize :: String -> T.Text -> Either T.Text [Token]
tokenize inname input = case parse tokenizer inname input of
        Right val -> Right val
        Left err -> Left (T.pack . show $ err)

tokenizer = many ngltoken <* eof
ngltoken = comment
        <|> symbol
        <|> tstring
        <|> word
        <|> operator
        <|> indent
        <|> taberror
        <|> eol

eol = char '\n' *> return TNewLine

try_string s = try (string s)

symbol = (char ':')  *> (TExpr . ConstSymbol . T.pack <$> variableStr) <* (char ':')
tstring = char '"' *> (TExpr . ConstStr <$> strtext) <* char '"'
strtext = T.pack <$> many (escapedchar <|> noneOf "\"")
    where
        escapedchar = char '\\' *> escapedchar'
        escapedchar' = do
            c <- anyChar
            case c of
                'n' -> return '\n'
                't' -> return '\t'
                '\\' -> return '\\'
                _ -> return c
comment = singlelinecomment <|> multilinecomment
singlelinecomment = commentstart *> skiptoeol
    where commentstart = (void $ char '#') <|> (void . try $ string "//")
skiptoeol = eol  <|> (anyChar *> skiptoeol)
multilinecomment = (try_string "/*") *> (TComment <$> skipmultilinecomment)
skipmultilinecomment = (try_string "*/" *> pure 0)
            <|> (char '\n' *> ((+1) <$> skipmultilinecomment))
            <|> (anyChar *> skipmultilinecomment)
            <|> (eof *> fail "Unexpected End Of File inside a comment")

word = try $ do
    k <- variableStr
    let k' = pure (T.pack k)
    if k `elem` reservedwords
        then TReserved <$> k'
        else TWord <$> k'
reservedwords = 
        ["if"
        ,"ngless"
        ,"len"
        ,"discard"
        ]

variableStr = (:) <$> (char '_' <|> letter) <*> many alphaNum
operator = TOperator <$> oneOf ",+-*():"
indent = TIndent . length <$> many1 (char ' ')
taberror = tab *> fail "Tabs are not used in NGLess. Please use spaces."
