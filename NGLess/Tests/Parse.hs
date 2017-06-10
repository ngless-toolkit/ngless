{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Parse
    ( tgroup_Parse
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Parsec (SourcePos, parse)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.Parsec.Pos (newPos)
import qualified Data.Text as T

import Tests.Utils
import Parse
import Tokens
import Language
import Utils.Here

tgroup_Parse = $(testGroupGenerator)
--
-- Test Parsing Module
parseText :: GenParser (SourcePos,Token) () a -> T.Text -> a
parseText p t = fromRight . parse p "test" . _cleanupindents . fromRight . tokenize "test" $ t
parseBody = map snd . parseText _nglbody

case_parse_symbol = parseBody "{symbol}" @?= [ConstSymbol "symbol"]
case_parse_fastq = parseBody fastqcalls @?= fastqcall
    where
        fastqcalls = "fastq(\"input.fq\")"
        fastqcall  = [FunctionCall (FuncName "fastq") (ConstStr "input.fq") [] Nothing]

case_parse_paired = parseBody fastqcalls @?= fastqcall
    where
        fastqcalls = "paired(\"input.fq\", \"pair.fq\")"
        fastqcall  = [FunctionCall (FuncName "paired") (ConstStr "input.fq") [(Variable "second", ConstStr "pair.fq")] Nothing]

case_parse_count = parseBody countcalls @?= countcall
    where
        countcalls = "count(annotated, count={gene})"
        countcall  = [FunctionCall (FuncName "count") (Lookup Nothing (Variable "annotated")) [(Variable "count", ConstSymbol "gene")] Nothing]

case_parse_count_mult_counts = parseBody countcalls @?= countcall
    where
        countcalls = "count(annotated, count=[{gene},{cds}])"
        countcall  = [FunctionCall (FuncName "count") (Lookup Nothing (Variable "annotated")) [(Variable "count", ListExpression [ConstSymbol "gene", ConstSymbol "cds"])] Nothing]

case_parse_assignment =  parseBody "reads = \"something\"" @?=
        [Assignment (Variable "reads") (ConstStr "something")]

case_parse_sequence = parseBody seqs @?= seqr
    where
        seqs = "reads = 'something'\nreads = 'something'"
        seqr = [a,a]
        a    = Assignment (Variable "reads") (ConstStr "something")

case_parse_num = parseBody nums @?= num
    where
        nums = "a = 0x10"
        num  = [Assignment (Variable "a") (ConstInt 16)]

case_parse_double = parseBody double_a_s @?= double_a
    where
        double_a_s = "d = 1.2"
        double_a = [Assignment (Variable "d") (ConstDouble 1.2)]

case_parse_bool = parseBody bools @?= bool
    where
        bools = "a = true"
        bool  = [Assignment (Variable "a") (ConstBool True)]

case_parse_if_else = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\nelse:\n 2\n"
        block  = [Condition (ConstBool True) (Sequence [ConstInt 0,ConstInt 1]) (Sequence [ConstInt 2])]

case_parse_if = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\n"
        block  = [Condition (ConstBool True) (Sequence [ConstInt 0,ConstInt 1]) (Sequence [])]

case_parse_if_end = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\n2\n"
        block  = [Condition (ConstBool True) (Sequence [ConstInt 0,ConstInt 1]) (Sequence []),ConstInt 2]

case_parse_ngless = parsengless "test" True ngs @?= Right ng
    where
        ngs = "ngless '0.0'\n"
        ng  = Script (Just $ Header "0.0" []) []

case_parse_import = parsengless "test" True ngs @?= Right ng
    where
        ngs = "ngless '0.0'\nimport 'testing' version '3.2-x'\n"
        ng  = Script (Just $ Header "0.0" [ModInfo "testing" "3.2-x"]) []

case_parse_comment_before_import = isOk "comments after ngless line should not fail (regression test)" $ parsengless "test" True [here|
ngless "0.0"
# This should not fail
import "parallel" version "0.0"

# This should not fail either
sample = lock1(readlines('input.txt'))
input = fastq(sample)
|]

case_parse_empty_line_before_import = isOk "empty lineafter ngless line should not fail (regression test)" $ parsengless "test" True [here|
ngless "0.0"

import "parallel" version "0.0"

sample = lock1(readlines('input.txt'))
input = fastq(sample)
|]

case_parse_list = parseText _listexpr "[a,b]" @?= ListExpression [Lookup Nothing (Variable "a"), Lookup Nothing (Variable "b")]

case_parse_indexexpr_11 = parseText _indexexpr "read[1:1]" @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexTwo j1 j1)
case_parse_indexexpr_10 = parseText _indexexpr "read[1:]"  @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexTwo j1 Nothing)
case_parse_indexexpr_01 = parseText _indexexpr "read[:1]"  @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexTwo Nothing j1)
case_parse_indexexpr_00 = parseText _indexexpr "read[:]"   @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexTwo Nothing Nothing)

case_parse_indexexprone_1 = parseText _indexexpr "read[1]" @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexOne (ConstInt 1))
case_parse_indexexprone_2 = parseText _indexexpr "read[2]" @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexOne (ConstInt 2))
case_parse_indexexprone_var = parseText _indexexpr "read[var]" @?= IndexExpression (Lookup Nothing (Variable "read")) (IndexOne (Lookup Nothing (Variable "var")))

case_parse_cleanupindents_0 = tokcleanupindents [TIndent 1] @?= []
case_parse_cleanupindents_1 = tokcleanupindents [TNewLine] @?= [TNewLine]
case_parse_cleanupindents_2 = tokcleanupindents [TIndent 1,TNewLine] @?= [TNewLine]
case_parse_cleanupindents_3 = tokcleanupindents [TOperator '(',TNewLine,TIndent 2,TOperator ')'] @?= [TOperator '(',TOperator ')']

case_parse_cleanupindents_4 = tokcleanupindents toks @?= toks'
    where
        toks  = [TWord "write",TOperator '(',TWord "A",TOperator ',',TNewLine,TIndent 16,TNewLine,TIndent 16,TWord "format",TOperator '=',TExpr (ConstSymbol "csv"),TOperator ')',TNewLine]
        toks' = [TWord "write",TOperator '(',TWord "A",TOperator ','                                        ,TWord "format",TOperator '=',TExpr (ConstSymbol "csv"),TOperator ')',TNewLine]
case_parse_cleanupindents_4' = tokcleanupindents toks @?= toks'
    where
        toks  = [TOperator '(',TOperator ',',TNewLine,TIndent 16,TNewLine,TIndent 16,TOperator ')',TNewLine]
        toks' = [TOperator '(',TOperator ','                                        ,TOperator ')',TNewLine]
case_parse_cleanupindents_4'' = tokcleanupindents toks @?= toks'
    where
        toks  = [TOperator '(',TNewLine,TIndent 16,TNewLine,TIndent 16,TOperator ')',TNewLine]
        toks' = [TOperator '('                                        ,TOperator ')',TNewLine]

j1 = Just (ConstInt 1)
tokcleanupindents = map snd . _cleanupindents . map (newPos "test" 0 0,)

case_parse_kwargs = parseBody "unique(reads,maxCopies=2)\n" @?= [FunctionCall (FuncName "unique") (Lookup Nothing (Variable "reads")) [(Variable "maxCopies", ConstInt 2)] Nothing]

case_parse_methods_kwargs_only = parseBody "sf.filter(min_identity_pc=90)\n" @?= [MethodCall (MethodName "filter") (Lookup Nothing (Variable "sf")) Nothing [(Variable "min_identity_pc", ConstInt 90)]]

