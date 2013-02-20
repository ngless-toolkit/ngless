{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parsengless
    ) where

import Language
import Tokens

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad.Identity ()
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Prim hiding (Parser)
import Text.Parsec.Combinator
import Text.Parsec (incSourceLine)

-- main function of this module
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict Text) before parsing
parsengless :: String -> T.Text -> Either T.Text Expression
parsengless inputname input =
        tokenize inputname input >>= parsetoks inputname

parsetoks :: String -> [Token] -> Either T.Text Expression
parsetoks inputname toks = case parse nglparser inputname toks of
            Right val -> Right val
            Left err -> Left (T.pack . show $ err)

type Parser = GenParser Token ()

nglparser = Sequence <$> (many1 expression)
expression :: Parser Expression
expression = assignment <|> rawexpr <|> funccall

tokf ::  (Token -> Maybe a) -> Parser a
tokf f = tokenPrim show updatePos f
    where
        updatePos s TNewLine _ = incSourceLine s 1
        updatePos s (TComment t) _ = incSourceLine s t
        updatePos s _ _ = s

rawexpr = tokf $ \t -> case t of
    TExpr e -> Just e
    _ -> Nothing

operator op = tokf $ \t -> case t of
    TOperator op' | op == op' -> Just t
    _ -> Nothing

word = tokf $ \t -> case t of
    TWord w -> Just w
    _ -> Nothing

funccall = FunctionCall <$>
                (try funcname <* operator '(')
                <*> ((:[]) <$> expression)
                <*> (kwargs <* operator ')')
                <*> pure Nothing

funcname = do
    fname <- word
    case fname of
        "fastq" -> pure Ffastq
        "substrim" -> pure Fsubstrim
        "map" -> pure Fmap
        "count" -> pure Fcount
        "write" -> pure Fwrite
        "print" -> pure Fprint
        _ -> fail "Function not found"

kwargs = many (operator ',' *> kwarg)
kwarg = (,) <$> (Variable <$> word <* operator '=') <*> expression

assignment = try assignment'
    where assignment' =
            Assignment <$> (Variable <$> word) <*> (space *> operator '=' *> space *> expression)

space = optional . tokf $ \t -> case t of { TComment _ -> Just (); TNewLine -> Just (); TIndent _ -> Just (); _ -> Nothing; }
