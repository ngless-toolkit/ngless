{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

-- | This module handles tokenization
module Tokens
    ( Token(..)
    , tokenize
    , _eol
    ) where

import qualified Data.Text as T
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad (void)
import Text.Parsec.Text ()
import Text.Parsec.Combinator
import Text.Parsec
import Language

-- | Token datatype
data Token =
        TExpr Expression -- ^ These are Const* type of expressions
        | TBop BOp -- ^ Binary operators
        | TReserved T.Text -- ^ Reserved keywords
        | TWord T.Text -- ^ Any other word (function name, variable, &c)
        | TOperator Char -- ^ A single character operator (',', '[', ...)
        | TNewLine -- ^ A new line
        | TIndent Int -- ^ A sequence of spaces
    deriving (Eq, Show)

-- | tokenize is the main function of this module
-- It returns tokens with position information (the position in the original
-- file)
tokenize :: String -- ^ input filename (for error messages)
        -> T.Text -- ^ input text
        -> Either T.Text [(SourcePos,Token)] -- ^ either error message or tokens
tokenize inname input = case parse tokenizer inname input of
        Right val -> Right val
        Left err -> Left (T.pack . show $ err)

tokenizer = many ngltoken' <* eof
ngltoken' = (,) <$> getPosition <*> ngltoken

-- | 'ngltoken' parse a token as a series of possibilities.
-- All of these are written so that they never fail after consuming input
ngltoken = comment
        <|> symbol
        <|> tstring
        <|> number
        <|> word
        <|> boperator
        <|> operator
        <|> indent
        <|> taberror
        <|> _eol

_eol = ((char '\r' *> char '\n') <|> char '\n') *> pure TNewLine

try_string s = try (string s)

symbol = (char '{')  *> (TExpr . ConstSymbol . T.pack <$> variableStr) <* (char '}')
tstring = tstring' '\'' <|> tstring' '"'
    where tstring' term = char term *> (TExpr . ConstStr <$> strtext term) <* char term
strtext term = T.pack <$> many (escapedchar <|> noneOf [term])
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
skiptoeol = _eol  <|> (anyChar *> skiptoeol)
multilinecomment = (try_string "/*") *> skipmultilinecomment
skipmultilinecomment = (try_string "*/" *> pure (TIndent 0))
            <|> (anyChar *> skipmultilinecomment)
            <|> (eof *> fail "Unexpected End Of File inside a comment")

word = try $ do
    k <- variableStr
    let k' = pure (T.pack k)
    case k of
        "true" -> pure (TExpr $ ConstBool True)
        "True" -> pure (TExpr $ ConstBool True)
        "false" -> pure (TExpr $ ConstBool False)
        "False" -> pure (TExpr $ ConstBool False)
        _
            | k `elem` reservedwords -> TReserved <$> k'
            | otherwise -> TWord <$> k'

reservedwords = 
        ["if"
        ,"else"
        ,"ngless"
        ,"len"
        ,"discard"
        ,"continue"
        ,"using"
        ]

variableStr = (:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum)
operator = TOperator <$> oneOf "=,+-*():[]<>.|"
boperator = choice [
                (try_string long *> pure (TBop short))
                    | (long,short) <-
                    [ ("!=", BOpNEQ)
                    , ("==", BOpEQ)
                    , ("<=", BOpLTE)
                    , ("<", BOpLT)
                    , (">=", BOpGTE)
                    , (">", BOpGT)

                    , ("+", BOpAdd)
                    , ("*", BOpMul)
                    ]]
indent = TIndent . length <$> many1 (char ' ')
taberror = tab *> fail "Tabs are not used in NGLess. Please use spaces."

number = TExpr . ConstNum <$> (try hex <|> dec)
    where
        hex = (read . ("0x"++)) <$> (string "0x" *> many1 hexDigit)
        dec = read <$> many1 digit
