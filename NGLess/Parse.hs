{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parsengless
    , _nglbody
    , _cleanupindents
    , _indexexpr
    , _listexpr
    ) where

import Language
import Tokens

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad.Identity ()
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Prim hiding (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Pos

-- | main function of this module
--
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict 'Text') before parsing
parsengless :: String -- ^ input filename (for error messages)
            -> T.Text -- ^ input data
            -> Either T.Text Script -- ^ either error message or parsed 'Script'
parsengless inputname input =
        tokenize inputname input >>= parsetoks inputname

parsetoks :: String -> [(SourcePos,Token)] -> Either T.Text Script
parsetoks inputname toks = case parse nglparser inputname (_cleanupindents toks) of
            Right val -> Right val
            Left err -> Left (T.pack . show $ err)

-- | '_cleanupindents' removes spaces that do not follow new lines as well as
-- any spaces that are between brackets (round or square).
_cleanupindents ::[(SourcePos,Token)] -> [(SourcePos, Token)]
_cleanupindents = _cleanupindents' []
    where
        _cleanupindents' _ [] = []
        _cleanupindents' cs (t@(_,TOperator o):ts)
                | isOpen o = (t:(_cleanupindents' (closeOf o:cs) ts))
        _cleanupindents' (c:cs) (t@(_,TOperator c'):ts)
                | c' == c = (t:(_cleanupindents' cs ts))
        _cleanupindents' cs@(_:_) ((_,TNewLine):ts) = _cleanupindents' cs ts
        _cleanupindents' cs@(_:_) ((_,TIndent _):ts) = _cleanupindents' cs ts
        _cleanupindents' [] ((_,TNewLine):(_,TIndent _):t0@(_,TNewLine):ts) = _cleanupindents' [] (t0:ts)
        _cleanupindents' [] (t0@(_,TNewLine):t1@(_,TIndent _):ts) = (t0:t1:_cleanupindents' [] ts)
        _cleanupindents' [] ((_,TIndent _):ts) = _cleanupindents' [] ts
        _cleanupindents' cs (t:ts) = (t:_cleanupindents' cs ts)

        isOpen '(' = True
        isOpen '[' = True
        isOpen _ = False
        closeOf '[' = ']'
        closeOf '(' = ')'
        closeOf _ = error "we should not close anything but [ & ("

type Parser = GenParser (SourcePos,Token) ()

nglparser = Script <$> ngless_version <*> (many eol *> _nglbody)
_nglbody = (eof *> pure []) <|> (many1 lno_expression <* eof)
lno_expression = (,) <$> linenr <*> expression
    where linenr = sourceLine `fmap` getPosition

expression :: Parser Expression
expression = expression' <* (many eol)
    where
        expression' =
                    conditional
                    <|> (reserved "discard" *> pure Discard)
                    <|> (reserved "continue" *> pure Continue)
                    <|> assignment
                    <|> funccall
                    <|> innerexpression

innerexpression = binoperator
                    <|> _listexpr
                    <|> _indexexpr
                    <|> base_expression

base_expression = pexpression
                    <|> rawexpr
                    <|> uoperator
                    <|> (Lookup <$> variable)

pexpression = operator '(' *> expression <* operator ')'

tokf ::  (Token -> Maybe a) -> Parser a
tokf f = token (show .snd) fst (f . snd)

rawexpr = tokf $ \t -> case t of
    TExpr e -> Just e
    _ -> Nothing
string = (tokf $ \t -> case t of { TExpr (ConstStr n) -> Just n; _ -> Nothing }) <?> "String"

operator op = (tokf $ \t -> case t of { TOperator op' | op == op' -> Just t; _ -> Nothing }) <?> (concat ["operator ", [op]])

word = tokf $ \t -> case t of
    TWord w -> Just w
    _ -> Nothing

reserved r = (tokf $ \t -> case t of { TReserved r' | r == r' -> Just r; _ -> Nothing }) <?> (concat [T.unpack r, " (reserved word)"])

indentation = (tokf $ \t -> case t of { TIndent i -> Just i; _ -> Nothing }) <?> "indentation"
eol = (tokf $ \t -> case t of { TNewLine -> Just (); _ -> Nothing }) <?> "end of line"
binop = (tokf $ \t -> case t of { TBop b -> Just b; _ -> Nothing }) <?> "binary operator"

uoperator = lenop <|> unary_minus
    where
        lenop = UnaryOp UOpLen <$> (reserved "len" *> operator '(' *> expression <* operator ')')
        unary_minus = UnaryOp UOpMinus <$> (operator '-' *> base_expression)
funccall = FunctionCall <$>
                (try funcname <* operator '(')
                <*> expression
                <*> (kwargs <* operator ')')
                <*> funcblock

funcblock = (Just <$> (Block <$> (reserved "using" *> operator '|' *> variableList <* operator '|' <* operator ':') <*> block))
    <|> pure Nothing

funcname = funcname' <?> "function name"
    where
        funcname' = do
            fname <- word
            case fname of
                "fastq" -> pure Ffastq
                "substrim" -> pure Fsubstrim
                "preprocess" -> pure Fpreprocess
                "map" -> pure Fmap
                "count" -> pure Fcount
                "unique" -> pure Funique
                "write" -> pure Fwrite
                "print" -> pure Fprint
                "annotate" -> pure Fannotate
                _ -> fail "Function not found"

kwargs = many (operator ',' *> kwarg) <?> "keyword argument list"
kwarg = kwarg' <?> "keyword argument"
    where kwarg' = (,) <$> (Variable <$> word <* operator '=') <*> expression

assignment = try assignment'
    where assignment' =
            Assignment <$> (Variable <$> word) <*> (operator '=' *> expression)

binoperator = try $ do
    a <- base_expression
    bop <- binop
    b <- expression
    return $ BinaryOp bop a b

_indexexpr = try (IndexExpression <$> base_expression <*> indexing)
    where
        indexing = try (IndexTwo <$> (operator '[' *> may_int <* operator ':') <*> (may_int <* operator ']'))
                    <|> (IndexOne <$> (operator '[' *> expression <* operator ']'))
        may_int = (Just <$> expression) <|> (pure Nothing)

_listexpr = try listexpr
    where
        listexpr = (operator '[') *> (ListExpression <$> (innerexpression `sepEndBy` (operator ','))) <* (operator ']')

conditional = Condition <$> (reserved "if" *> expression <* operator ':') <*> block <*> mayelse
mayelse = elseblock <|> (pure $ Sequence [])
elseblock = try (many indentation *> reserved "else" *> operator ':' *> block)
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
variableList = sepBy1 variable (operator ',') <?> "variable list"
variable = Variable <$> word <?> "variable"

ngless_version = ngless_version' <?> "ngless version declararion"
    where ngless_version' = reserved "ngless" *> (string <?> "ngless version string") <* eol

