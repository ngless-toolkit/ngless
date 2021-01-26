{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, CPP #-}

-- | This module handles tokenization
module Tokens
    ( Token(..)
    , tokenize
#ifdef IS_BUILDING_TEST
    , eol
#endif
    ) where

import qualified Data.Text as T
import Control.Monad (void)
import Text.Parsec.Text ()
import Text.Parsec.Combinator
import Data.Functor (($>))
import Text.Parsec

import NGLess.NGError
import Language

-- | Token datatype
data Token =
        TExpr Expression -- ^ These are Const* type of expressions
        | TBop BOp -- ^ Binary operators
        | TReserved T.Text -- ^ Reserved keywords
        | TConstant T.Text -- ^ Builtin constant
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
        -> NGLess [(SourcePos,Token)]
tokenize inname input = case parse tokenizer inname input of
        Right val -> return val
        Left err -> throwScriptError $ show err

tokenizer = many ngltoken' <* eof
ngltoken' = (,) <$> getPosition <*> ngltoken

-- | 'ngltoken' parse a token as a series of possibilities.
-- All of these are written so that they never fail after consuming input
ngltoken = comment
        <|> symbol
        <|> tstring
        <|> double
        <|> integer
        <|> word
        <|> boperator
        <|> operator
        <|> indent
        <|> taberror
        <|> eol

eol = semicolon <|> real_eol
    where
        real_eol = ((char '\r' *> char '\n') <|> char '\n') $> TNewLine
        semicolon = char ';' *> skipMany (char ' ') $> TNewLine

try_string s = try (string s)

symbol = char '{'  *>
                (TExpr . ConstSymbol . T.pack <$> many1 (char '_' <|> alphaNum))
                <* (option () $ char '-' *> parserFail "Symbols cannot contain hyphens (-), perhaps you meant to use an underscore.")
                <* char '}'

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
skiptoeol = eol  <|> (anyChar *> skiptoeol)
multilinecomment = try_string "/*" *> skipmultilinecomment
skipmultilinecomment = (try_string "*/" $> TIndent 0)
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
            | k `elem` constants -> TExpr . BuiltinConstant . Variable <$> k'
            | otherwise -> TWord <$> k'

reservedwords =
        ["if"
        ,"else"
        ,"ngless"
        ,"len"
        ,"discard"
        ,"continue"
        ,"not"
        ,"local"
        ,"import"
        ,"using"
        ]

constants =
        ["STDIN"
        ,"STDOUT"
        ]

variableStr = (:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum)
operator = TOperator <$> oneOf "=,+-*():[]<>.|"
boperator =
        -- Check for a not-so-unreasonable user mistake
        (try_string "<\\>" *> fail "<\\> found. Did you mean </>?")
        <|> choice [
                (try_string long $> TBop short)
                    | (long,short) <-
                    [ ("!=", BOpNEQ)
                    , ("==", BOpEQ)

                    , ("</>", BOpPathAppend)

                    , ("<=", BOpLTE)
                    , ("<", BOpLT)
                    , (">=", BOpGTE)
                    , (">", BOpGT)

                    , ("+", BOpAdd)
                    , ("*", BOpMul)
                    ]]
indent = TIndent . length <$> many1 (char ' ')
taberror = tab *> fail "Tabs are not used in NGLess. Please use spaces."

double = TExpr . ConstDouble . read <$> try double'
    where
        double' = (\int frac -> (int ++ "." ++ frac)) <$> many1 digit <*> (char '.' *> many1 digit)

integer = TExpr . ConstInt <$> (try hex <|> dec)
    where
        hex = read . ("0x"++) <$> (string "0x" *> many1 hexDigit)
        dec = read <$> many1 digit
