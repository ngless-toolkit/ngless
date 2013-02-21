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
import Text.Parsec (SourcePos)

-- main function of this module
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict Text) before parsing
parsengless :: String -> T.Text -> Either T.Text Expression
parsengless inputname input =
        tokenize inputname input >>= parsetoks inputname

parsetoks :: String -> [(SourcePos,Token)] -> Either T.Text Expression
parsetoks inputname toks = case parse nglparser inputname (cleanupindents toks) of
            Right val -> Right val
            Left err -> Left (T.pack . show $ err)

cleanupindents ::[(SourcePos,Token)] -> [(SourcePos, Token)]
cleanupindents ((p0,TNewLine):(p1,TIndent i):ts) = ((p0,TNewLine):(p1,TIndent i):cleanupindents ts)
cleanupindents ((_,TIndent _):ts) = cleanupindents ts
cleanupindents (t:ts) = (t:cleanupindents ts)
cleanupindents [] = []

type Parser = GenParser (SourcePos,Token) ()


nglparser = Sequence <$> (many1 expression <* eof)
expression :: Parser Expression
expression = expression' <* (many eol)
    where
        expression' =
                    conditional
                    <|> ngless_version
                    <|> assignment
                    <|> binoperator
                    <|> funccall
                    <|> base_expression

base_expression = pexpression
                    <|> rawexpr
                    <|> (Lookup <$> variable)

pexpression = operator '(' *> expression <* operator ')'

tokf ::  (Token -> Maybe a) -> Parser a
tokf f = token (show .snd) fst (f . snd)

rawexpr = tokf $ \t -> case t of
    TExpr e -> Just e
    _ -> Nothing
integer = tokf $ \t -> case t of
    TExpr (ConstNum n) -> Just n
    _ -> Nothing

operator op = tokf $ \t -> case t of
    TOperator op' | op == op' -> Just t
    _ -> Nothing

word = tokf $ \t -> case t of
    TWord w -> Just w
    _ -> Nothing

reserved r = tokf $ \t -> case t of
    TReserved r' | r == r' -> Just r
    _ -> Nothing

indentation = tokf $ \t -> case t of
    TIndent i -> Just i
    _ -> Nothing

eol = tokf $ \t -> case t of { TNewLine -> Just (); _ -> Nothing }
binop = tokf $ \t -> case t of { TBop b -> Just b; _ -> Nothing }

funccall = FunctionCall <$>
                (try funcname <* operator '(')
                <*> ((:[]) <$> expression)
                <*> (kwargs <* operator ')' <* space)
                <*> funcblock

funcblock = (Just <$> (Block <$> (reserved "using" *> space *> operator '|' *> variableList <* operator '|' <* operator ':') <*> block))
    <|> pure Nothing

funcname = do
    fname <- word
    case fname of
        "fastq" -> pure Ffastq
        "substrim" -> pure Fsubstrim
        "process_reads" -> pure Fprocess_reads
        "map" -> pure Fmap
        "count" -> pure Fcount
        "unique" -> pure Funique
        "write" -> pure Fwrite
        "print" -> pure Fprint
        _ -> fail "Function not found"

kwargs = many (operator ',' *> kwarg)
kwarg = (,) <$> (Variable <$> word <* operator '=') <*> expression

assignment = try assignment'
    where assignment' =
            Assignment <$> (Variable <$> word) <*> (space *> operator '=' *> space *> expression)

binoperator = try $ do
    a <- base_expression
    bop <- space *> binop <* space
    b <- expression
    return $ BinaryOp bop a b


space = optional . tokf $ \t -> case t of { TNewLine -> Just (); TIndent _ -> Just (); _ -> Nothing; }

conditional = Condition <$> (reserved "if" *> expression <* operator ':') <*> block <*> mayelse
mayelse = elseblock <|> (pure $ Sequence [])
elseblock = (reserved "else" *> operator ':' *> block)
block = do
        eol
        level <- indentation
        first <- expression <* many eol
        rest <- block' level
        return $ Sequence (first:rest)
    where
        block' level = many (try $ do
                            level' <- indentation
                            if level /= level'
                                then fail "indentation changed"
                                else expression <* many eol)
variableList = sepBy1 variable (operator ',' *> space)
variable = Variable <$> word

ngless_version = NGLessVersion <$> (reserved "ngless" *> space *> integer) <*> (operator '.' *> integer)
