{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

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
import Text.Parsec.Error

sliceList :: Int -> Int -> [a] -> [a]
sliceList st e = take (e - st) . drop st

-- | main function of this module
--
-- Because the scripts are expected to be small, we can expect to load them
-- whole into memory (with a strict 'Text') before parsing
parsengless :: String -- ^ input filename (for error messages)
            -> Bool -- ^ whether the version statement is mandatory
            -> T.Text -- ^ input data
            -> Either T.Text Script -- ^ either error message or parsed 'Script'
parsengless inputname reqversion input = tokenize inputname input >>= parsetoks input inputname reqversion

parsetoks :: T.Text -> String -> Bool -> [(SourcePos,Token)] -> Either T.Text Script
parsetoks input inputname reqversion toks = case parse (nglparser reqversion) inputname (_cleanupindents toks) of
            Right val -> Right val
            Left err -> Left $ buildErrorMessage input err


buildErrorMessage :: T.Text -> ParseError -> T.Text
buildErrorMessage input err = T.concat $ ["Parsing error on file '", T.pack fname, "' on line ", T.pack . show $ line, " (column ", T.pack . show $ col, ")\n\n"]
                    ++ preLines 3
                    ++ ["\n", indicatorLine]
                    ++ postLines 2
                    ++ ["\n\n", T.pack . show $ err, "\n"]
    where
        pos = errorPos err
        fname = sourceName pos
        line = sourceLine pos
        col = sourceColumn pos
        sourceLines = T.lines input
        preLines :: Int -> [T.Text]
        preLines n = withNLTAB $ sliceList (max 0 (line - n)) (line + 1) sourceLines
        postLines n = withNLTAB $ sliceList (line + 1) (line + 1 + n) sourceLines
        withNLTAB [] = []
        withNLTAB (ell:ls) = ("\n\t":ell:withNLTAB ls)
        indicatorLine = T.pack (['-' | _ <- [1..col+8]] ++ "^")


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

nglparser False = Script <$> optionMaybe ngless_header <*> (many eol *> _nglbody)
nglparser True = Script <$> (Just <$> ngless_header) <*> (many eol *> _nglbody)
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
                    <|> innerexpression

innerexpression = binoperation_or_just_left
left_expression =  uoperator
                    <|> method_call
                    <|> _indexexpr
                    <|> base_expression

base_expression = pexpression
                    <|> (funccall <?> "function call")
                    <|> _listexpr
                    <|> rawexpr
                    <|> (Lookup <$> variable)

pexpression = operator '(' *> expression <* operator ')'

tokf ::  (Token -> Maybe a) -> Parser a
tokf f = token (show .snd) fst (f . snd)

rawexpr = tokf $ \case
    TExpr e -> Just e
    _ -> Nothing
string = (tokf $ \case { TExpr (ConstStr n) -> Just n; _ -> Nothing }) <?> "String"

operator op = (tokf $ \t -> case t of { TOperator op' | op == op' -> Just t; _ -> Nothing }) <?> (concat ["operator ", [op]])

word = tokf $ \case
    TWord w -> Just w
    _ -> Nothing

match_word w = (tokf $ \case
    TWord w' | w == w' -> Just w
    _ -> Nothing) <?> ("word "++T.unpack w)
reserved r = (tokf $ \case { TReserved r' | r == r' -> Just r; _ -> Nothing }) <?> (concat [T.unpack r, " (reserved word)"])

indentation = (tokf $ \case { TIndent i -> Just i; _ -> Nothing }) <?> "indentation"
eol = (tokf $ \case { TNewLine -> Just (); _ -> Nothing }) <?> "end of line"
binop = (tokf $ \case { TBop b -> Just b; _ -> Nothing }) <?> "binary operator"

uoperator = lenop <|> unary_minus <|> not_expr
    where
        lenop = UnaryOp UOpLen <$> (reserved "len" *> operator '(' *> expression <* operator ')')
        unary_minus = UnaryOp UOpMinus <$> (operator '-' *> base_expression)
        not_expr = UnaryOp UOpNot <$> (reserved "not" *> innerexpression)
funccall = try paired <|> FunctionCall <$>
                try (funcname <* operator '(')
                <*> innerexpression
                <*> (kwargs <* operator ')')
                <*> funcblock

funcblock = optionMaybe (Block <$> (reserved "using" *> operator '|' *> variableList <* operator '|' <* operator ':') <*> block)

paired = FunctionCall
            <$> (match_word "paired" *> pure Fpaired)
            <*> (operator '(' *> innerexpression <* operator ',')
            <*> pairedKwArgs
            <*> pure Nothing

funcname = funcname' <?> "function name"
    where
        funcname' = do
            fname <- word
            case fname of
                "fastq" -> pure Ffastq
                "samfile" -> pure Fsamfile
                "substrim" -> pure Fsubstrim
                "preprocess" -> pure Fpreprocess
                "map" -> pure Fmap
                "count" -> pure Fcount
                "select" -> pure Fselect
                "unique" -> pure Funique
                "write" -> pure Fwrite
                "print" -> pure Fprint
                "annotate" -> pure Fannotate
                _ -> pure (Fother fname)

methodName = methodName' <?> "method name"
    where
        methodName' = do
            mname <- word
            case mname of
                "flag" -> pure Mflag
                "score" -> pure Mscore
                _ -> fail "Method name not found"

pairedKwArgs = (++) <$> (wrap <$> expression) <*> (kwargs <* operator ')')
    where wrap e = [(Variable "second", e)]

kwargs = many (operator ',' *> kwarg) <?> "keyword argument list"
kwarg = kwarg' <?> "keyword argument"
    where kwarg' = (,) <$> (variable <* operator '=') <*> expression

assignment = try assignment'
    where assignment' =
            Assignment <$> variable <*> (operator '=' *> expression)

binoperation_or_just_left = try $ do
    left <- left_expression
    (try $ do {
        bop <- binop;
        right <- innerexpression;
        return $ BinaryOp bop left right}) <|> return left

method_call = try $ do
    self <- base_expression <* operator '.'
    met <- methodName <* operator '('
    a <- optionMaybe expression
    kws <- kwargs <* operator ')'
    return (MethodCall met self a kws)

_indexexpr = try (IndexExpression <$> base_expression <*> indexing)
    where
        indexing = try (IndexTwo <$> (operator '[' *> may_int <* operator ':') <*> (may_int <* operator ']'))
                    <|> (IndexOne <$> (operator '[' *> innerexpression <* operator ']'))
        may_int = optionMaybe innerexpression

_listexpr = try listexpr
    where
        listexpr = (operator '[') *> (ListExpression <$> (innerexpression `sepEndBy` (operator ','))) <* (operator ']')

conditional = Condition <$> (reserved "if" *> innerexpression <* operator ':') <*> block <*> mayelse
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
variableList = sepBy1 variable (operator ',') <?> "variable list"
variable = Variable <$> word <?> "variable"

ngless_header = Header <$> ngless_version <*> many import_mod
ngless_version = ngless_version' <?> "ngless version declararion"
    where ngless_version' = reserved "ngless" *> (string <?> "ngless version string") <* eol

import_mod = ModInfo <$> (reserved "import" *> (string <?> "module name")) <*> (match_word "version" *> (string <?> "module version")) <* eol

