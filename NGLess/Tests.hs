{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Control.Applicative
import Text.Parsec (parse)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import qualified Data.Text as T
import Text.Parsec (SourcePos)

import Language
import Parse
import Tokens

-- The main test driver is automatically generated
main = $(defaultMainGenerator)


-- Test Parsing Module
parseText :: GenParser (SourcePos,Token) () a -> T.Text -> a
parseText p t = fromRight . parse p "test" . fromRight . tokenize "test" $ t
fromRight (Right r) = r
fromRight (Left _) = undefined

case_parse_symbol = parsengless "test" "{symbol}" @?= Right (Sequence [ConstSymbol "symbol"])
case_parse_fastq = parsengless "test" fastqcalls @?= Right fastqcall
    where
        fastqcalls = "fastq(\"input.fq\")"
        fastqcall  = Sequence [FunctionCall Ffastq [ConstStr "input.fq"] [] Nothing]

case_parse_assignment =  parsengless "test" "reads = \"something\"" @?=
        Right (Sequence [Assignment (Variable "reads") (ConstStr "something")])


case_parse_sequence = parsengless "test" seqs @?= Right seqr
    where
        seqs = "reads = 'something'\nreads = 'something'"
        seqr = Sequence [a,a]
        a    = Assignment (Variable "reads") (ConstStr "something")

case_parse_num = parsengless "test" nums @?= Right num
    where
        nums = "a = 0x10"
        num  = Sequence [Assignment (Variable "a") (ConstNum 16)]

case_parse_bool = parsengless "test" bools @?= Right bool
    where
        bools = "a = true"
        bool  = Sequence [Assignment (Variable "a") (ConstBool True)]

case_parse_if_else = parsengless "test" blocks @?= Right block
    where
        blocks = "if true:\n 0\n 1\nelse:\n 2\n"
        block  = Sequence [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence [ConstNum 2])]

case_parse_if = parsengless "test" blocks @?= Right block
    where
        blocks = "if true:\n 0\n 1\n"
        block  = Sequence [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence [])]

case_parse_if_end = parsengless "test" blocks @?= Right block
    where
        blocks = "if true:\n 0\n 1\n2\n"
        block  = Sequence [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence []),ConstNum 2]

case_parse_ngless = parsengless "test" ngs @?= Right ng
    where
        ngs = "ngless 0.0\n"
        ng  = Sequence [NGLessVersion 0 0]

case_parse_indexexpr_11 = parseText _indexexpr "read[1:1]" @?= IndexExpression (Lookup (Variable "read")) (Index (Just 1) (Just 1))
case_parse_indexexpr_10 = parseText _indexexpr "read[1:]"  @?= IndexExpression (Lookup (Variable "read")) (Index (Just 1) Nothing)
case_parse_indexexpr_01 = parseText _indexexpr "read[:1]"  @?= IndexExpression (Lookup (Variable "read")) (Index Nothing (Just 1))
case_parse_indexexpr_00 = parseText _indexexpr "read[:]"   @?= IndexExpression (Lookup (Variable "read")) (Index Nothing Nothing)


-- Test Tokens module
tokenize' fn t = map snd <$> (tokenize fn t)

case_tok_cr = TNewLine @=? (case parse (_eol <* eof) "test" "\r\n" of { Right t -> t; Left _ -> error "Parse failed"; })
case_tok_single_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0# comment\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstNum 0),TNewLine,TWord "b",TOperator '=',TExpr (ConstNum 1),TNewLine]

case_tok_multi_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0/* This\n\nwith\nlines*/\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstNum 0),TIndent 0,TNewLine,TWord "b",TOperator '=',TExpr (ConstNum 1),TNewLine]

case_tok_word_ = tokenize' "test" "word_with_underscore" @?= Right expected
    where
        expected = [TWord "word_with_underscore"]
