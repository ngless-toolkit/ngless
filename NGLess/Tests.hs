{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules


import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Control.Applicative
import Text.Parsec (parse)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.Parsec (SourcePos)
import Text.Parsec.Pos (newPos)

import System.Directory(removeFile, removeDirectoryRecursive)
import System.FilePath.Posix((</>))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.Text as T

import Data.Aeson
import Data.Char

import qualified Data.IntervalMap.Strict as IM
import qualified Data.IntervalMap.Interval as IM

import qualified Data.Vector.Unboxed as V

import Language
import Interpret
import Parse
import Tokens
import Types
import Unique
import PrintFastqBasicStats
import Substrim
import FastQStatistics
import FileManagement
import Validation
import CountOperation
import Annotation
import ValidationNotPure
import SamBamOperations
import VectorOperations
import ProcessFastQ
import WriteInterpretOperation
import MapInterpretOperation
import ReferenceDatabases
import Configuration

import Data.Sam
import Data.Json
import Data.AnnotRes
import Data.DefaultValues
import qualified Data.GFF as GFF


-- The main test driver is automatically generated
main = $(defaultMainGenerator)

-- Test Parsing Module
parseText :: GenParser (SourcePos,Token) () a -> T.Text -> a
parseText p t = fromRight . parse p "test" . _cleanupindents . fromRight . tokenize "test" $ t
fromRight (Right r) = r
fromRight (Left e) = error (concat ["Unexpected Left: ",show e])
parseBody = map snd . parseText _nglbody
parsetest = parsengless "test"

case_parse_symbol = parseBody "{symbol}" @?= [ConstSymbol "symbol"]
case_parse_fastq = parseBody fastqcalls @?= fastqcall
    where
        fastqcalls = "fastq(\"input.fq\")"
        fastqcall  = [FunctionCall Ffastq (ConstStr "input.fq") [] Nothing]

case_parse_count = parseBody countcalls @?= countcall
    where
        countcalls = "count(annotated, count={gene})"
        countcall  = [FunctionCall Fcount (Lookup (Variable "annotated")) [(Variable "count",ConstSymbol "gene")] Nothing]

case_parse_count_mult_counts = parseBody countcalls @?= countcall
    where
        countcalls = "count(annotated, count=[{gene},{cds}])"
        countcall  = [FunctionCall Fcount (Lookup (Variable "annotated")) [(Variable "count", ListExpression [ConstSymbol "gene", ConstSymbol "cds"])] Nothing]

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
        num  = [Assignment (Variable "a") (ConstNum 16)]

case_parse_bool = parseBody bools @?= bool
    where
        bools = "a = true"
        bool  = [Assignment (Variable "a") (ConstBool True)]

case_parse_if_else = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\nelse:\n 2\n"
        block  = [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence [ConstNum 2])]

case_parse_if = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\n"
        block  = [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence [])]

case_parse_if_end = parseBody blocks @?= block
    where
        blocks = "if true:\n 0\n 1\n2\n"
        block  = [Condition (ConstBool True) (Sequence [ConstNum 0,ConstNum 1]) (Sequence []),ConstNum 2]

case_parse_ngless = parsengless "test" ngs @?= Right ng
    where
        ngs = "ngless '0.0'\n"
        ng  = Script "0.0" []

case_parse_list = parseText _listexpr "[a,b]" @?= ListExpression [Lookup (Variable "a"), Lookup (Variable "b")]

case_parse_indexexpr_11 = parseText _indexexpr "read[1:1]" @?= IndexExpression (Lookup (Variable "read")) (IndexTwo j1 j1)
case_parse_indexexpr_10 = parseText _indexexpr "read[1:]"  @?= IndexExpression (Lookup (Variable "read")) (IndexTwo j1 Nothing)
case_parse_indexexpr_01 = parseText _indexexpr "read[:1]"  @?= IndexExpression (Lookup (Variable "read")) (IndexTwo Nothing j1)
case_parse_indexexpr_00 = parseText _indexexpr "read[:]"   @?= IndexExpression (Lookup (Variable "read")) (IndexTwo Nothing Nothing)

case_parse_indexexprone_1 = parseText _indexexpr "read[1]" @?= IndexExpression (Lookup (Variable "read")) (IndexOne (ConstNum 1))
case_parse_indexexprone_2 = parseText _indexexpr "read[2]" @?= IndexExpression (Lookup (Variable "read")) (IndexOne (ConstNum 2))
case_parse_indexexprone_var = parseText _indexexpr "read[var]" @?= IndexExpression (Lookup (Variable "read")) (IndexOne (Lookup (Variable "var")))

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

j1 = Just (ConstNum 1)
tokcleanupindents = map snd . _cleanupindents . map (newPos "test" 0 0,)

case_parse_kwargs = parseBody "unique(reads,maxCopies=2)\n" @?= [FunctionCall Funique (Lookup (Variable "reads")) [(Variable "maxCopies", ConstNum 2)] Nothing]

-- Test Tokens module
tokenize' fn t = map snd <$> (tokenize fn t)

case_tok_cr = TNewLine @=? (case parse (_eol <* eof) "test" "\r\n" of { Right t -> t; Left _ -> error "Parse failed"; })
case_tok_single_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0# comment\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstNum 0),TNewLine,TWord "b",TOperator '=',TExpr (ConstNum 1),TNewLine]

case_tok_single_line_comment_cstyle = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0// comment\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstNum 0),TNewLine,TWord "b",TOperator '=',TExpr (ConstNum 1),TNewLine]

case_tok_multi_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0/* This\n\nwith\nlines*/\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstNum 0),TIndent 0,TNewLine,TWord "b",TOperator '=',TExpr (ConstNum 1),TNewLine]

case_tok_word_ = tokenize' "test" "word_with_underscore" @?= Right expected
    where
        expected = [TWord "word_with_underscore"]



-- Test Encoding
case_calculateEncoding_sanger = calculateEncoding 55 @?= Encoding "Sanger / Illumina 1.9" sanger_encoding_offset
case_calculateEncoding_illumina_1 = calculateEncoding 65 @?= Encoding "Illumina 1.3" illumina_encoding_offset
case_calculateEncoding_illumina_1_5 = calculateEncoding 100 @?= Encoding "Illumina 1.5" illumina_encoding_offset

--- SETUP to reduce imports.
-- test array: "\n\v\f{zo\n\v\NUL" -> [10,11,12,123,122,111,10,11,0]
-- test cutoff: chr 20 -> '\DC4'

--Property 1: For every s, the size must be allways smaller than the input
prop_substrim_maxsize s = st >= 0 && e <= B.length (B.pack s)
    where (st,e) = subtrimPos (B.pack s) '\DC4'

-- Property 2: substrim should be idempotent
prop_substrim_idempotent s = st == 0 && e == B.length s1
    where
        s1 = removeBps (B.pack s) (subtrimPos (B.pack s) '\DC4')
        (st,e) = subtrimPos s1 '\DC4'
                        
case_substrim_normal_exec =  subtrimPos "\n\v\f{zo\n\v\NUL" '\DC4' @?= (3,3)
case_substrim_empty_quals = subtrimPos "" '\DC4' @?= (0,0)

-- Test Types
isError (Right _) = assertFailure "error not caught"
isError (Left _) = return ()

isOk m (Left _) = assertFailure m
isOk _ (Right _) = return ()
isOkTypes = isOk "Type error on good code"

case_bad_type_fastq = isError $ checktypes (Script "0.0" [(0,FunctionCall Ffastq (ConstNum 3) [] Nothing)])
case_good_type_fastq = isOkTypes $ checktypes (Script "0.0" [(0,FunctionCall Ffastq (ConstStr "fastq.fq") [] Nothing)])

case_type_complete = isOkTypes $ (parsetest complete) >>= checktypes

complete = "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \reads = unique(reads,max_copies=2)\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    read = substrim(read, min_quality=24)\n\
    \    if len(read) < 30:\n\
    \        discard\n"

case_indent_comment = isOk "ParseFailed" $ parsetest indent_comment
case_indent_space = isOk "ParseFailed" $ parsetest indent_space

indent_comment = "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    # comment \n"

indent_space  = "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    \n"

case_indent_empty_line = isOkTypes $ parsetest indent_empty_line >>= checktypes
    where indent_empty_line  = "ngless '0.0'\n\
            \reads = fastq('input1.fq')\n\
            \preprocess(reads) using |read|:\n\
            \    read = read[5:]\n\
            \    \n\
            \    if len(read) < 24:\n\
            \        discard\n"

--- Check types in Function optional Arguments

-- fastq

case_invalid_func_fastq = isError $ checktypes f_expr
    where 
        f_expr = Script "0.0" [(0,FunctionCall Ffastq (ConstStr "fastq.fq") [(Variable "xpto", (ConstNum 10))] Nothing)]

-- unique

case_valid_funique_mc = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \unique(x, max_copies=10)\n"

case_invalid_funique_mc = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \unique(x, max_copies='test')\n"


-- substrim

case_valid_fsubstrim_mq = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                 \x = fastq('fq')\n\
                 \preprocess(x) using |read|:\n\
                 \    read = read[5:]\n\
                 \    read = substrim(read, min_quality=2)\n"

case_invalid_fsubstrim_mq = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                 \x = fastq('fq')\n\
                 \preprocess(x) using |read|:\n\
                 \    read = read[5:]\n\
                 \    read = substrim(read, min_quality='2')\n"

-- map

case_invalid_fmap_ref = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \map(x, reference=10)\n"


case_valid_fmap_ref = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \map(x, reference='xpto')\n"

-- annotate
case_valid_fannot_gff = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, gff='xpto')"

case_invalid_fannot_gff = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, gff={xpto})"

case_valid_fannot_mode = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, mode={union})"

case_invalid_fannot_mode = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, mode='union')"

case_valid_fannot_features = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, features=[{gene}])"

case_invalid_fannot_features = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \annotate(y, features='gene')"

-- count
case_valid_fcount_min = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \k = count(z, min=10)"

case_invalid_fcount_min = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \k = count(z, min='10')"


case_valid_fcount_counts = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \k = count(z, counts=[{gene}])"

case_invalid_fcount_counts = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \k = count(z, counts=['gene'])"

-- write

case_valid_fwrite_ofile = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \write(count(z), ofile='10')"

case_invalid_fwrite_ofile = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \write(count(z), ofile=10)"


case_valid_fwrite_format = isOkTypes $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \write(count(z), format={tsv})"

case_invalid_fwrite_format = isError $ parsetest f_attr >>= checktypes
    where 
        f_attr = "ngless '0.0'\n\
                  \x = fastq('fq')\n\
                  \y = map(x, reference='xpto')\n\
                  \z = annotate(y, features=[{gene}])\n\
                  \write(count(z), format='tsv')"


-- Validation non pure functions

-----------------------------------
----------  IMPORTANT -------------
-- File: Makefile allways exists.
-- File: fq never exists.
-----------------------------------

case_valid_not_pure_fp_fastq_lit = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \fastq('fq')\n"

case_invalid_not_pure_fp_fastq_lit = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \fastq('Makefile')\n" --File Makefile Allways Exists

case_valid_not_pure_fp_fastq_const = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \x = 'fq'\n\
                 \fastq(x)\n"

case_invalid_not_pure_fp_fastq_const = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \x = 'Makefile'\n\
                 \fastq(x)\n" --File Makefile Allways Exists


case_valid_not_pure_map_reference_lit = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \map(x, reference='Makefile')\n"

case_invalid_not_pure_map_def_reference_lit = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \map(x, reference='sacCer3')\n"

case_invalid_not_pure_map_reference_lit = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \map(x, reference='fq')\n"


case_valid_not_pure_map_reference_const = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'Makefile'\n\
                 \map(x, reference=v)\n"

case_invalid_not_pure_map_def_reference_const = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'sacCer3'\n\        
                 \map(x, reference=v)\n"

case_invalid_not_pure_map_reference_const = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'fq'\n\
                 \map(x, reference=v)\n"


case_valid_not_pure_annotate_gff_lit = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \annotate(x, gff='Makefile')\n"

case_invalid_not_pure_annotate_gff_lit = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \annotate(x, gff='fq')\n"


case_valid_not_pure_annotate_gff_const = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'Makefile'\n\
                 \annotate(x, gff=v)\n"

case_invalid_not_pure_annotate_gff_const = isError =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'fq'\n\
                 \annotate(x, gff=v)\n"



case_valid_not_pure_annotate_gff_const2 = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'fq'\n\
                 \v = 'Makefile'\n\
                 \annotate(x, gff=v)\n"

case_invalid_not_pure_annotate_gff_const2 = isOkTypes =<< validate_io' (fromRight . parsetest $ f_attr)
    where 
        f_attr = "ngless '0.0'\n\
                 \v = 'fq'\n\
                 \v = 'fq'\n\
                 \annotate(x, gff=v)\n"


case_validate_not_pure_io_script_idemp = do
    r <- validate_io script 
    r @?= script
  where 
      script = fromRight . parsetest $ f_attr
      f_attr = "ngless '0.0'\n\
                \v = 'Makefile'\n\
                \annotate(x, gff=v)\n"


--- Validation pure functions

case_bad_function_attr_count = isError $ parsetest function_attr >>= validate
    where function_attr = "ngless '0.0'\n\
            \count(annotated, count={gene})\n"

case_good_function_attr_count_1 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \write(count(annotated, counts=[{gene}]),ofile='gene_counts.csv',format={csv})"

case_good_function_attr_count_2 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \counts = count(annotated, counts=[{gene}])"

case_bad_function_attr_map = isError $ parsetest function_attr >>= validate
    where function_attr = "ngless '0.0'\n\
            \map(input,reference='sacCer3')\n"

case_good_function_attr_map_1 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \write(map(input,reference='sacCer3'),ofile='result.sam',format={sam})"

case_good_function_attr_map_2 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \counts = map(input,reference='sacCer3')"


-- Type Validate pre process operations

case_pre_process_indexation_1 = evalIndex (NGOShortRead "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 5), Nothing] @?= (NGOShortRead "@IRIS" "CAA" "aaa")
case_pre_process_indexation_2 = evalIndex (NGOShortRead "@IRIS" "AGTACCAA" "aa`aaaaa") [Nothing, Just (NGOInteger 3)] @?= (NGOShortRead "@IRIS" "AGT" "aa`")
case_pre_process_indexation_3 = evalIndex (NGOShortRead "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 2), Just (NGOInteger 5)] @?= (NGOShortRead "@IRIS" "TAC" "`aa")

case_pre_process_length_1 = evalLen (NGOShortRead "@IRIS" "AGTACCAA" "aa`aaaaa") @?= (NGOInteger 8)

case_bop_gte_1 = evalBinary BOpGTE (NGOInteger 10) (NGOInteger 10) @?= (NGOBool True)
case_bop_gte_2 = evalBinary BOpGTE (NGOInteger 11) (NGOInteger 10) @?= (NGOBool True)
case_bop_gte_3 = evalBinary BOpGTE (NGOInteger 10) (NGOInteger 11) @?= (NGOBool False)

case_bop_gt_1 = evalBinary BOpGT (NGOInteger 10) (NGOInteger 10) @?= (NGOBool False)
case_bop_gt_2 = evalBinary BOpGT (NGOInteger 11) (NGOInteger 10) @?= (NGOBool True)
case_bop_gt_3 = evalBinary BOpGT (NGOInteger 10) (NGOInteger 11) @?= (NGOBool False)

case_bop_lt_1 = evalBinary BOpLT (NGOInteger 10) (NGOInteger 10) @?= (NGOBool False)
case_bop_lt_2 = evalBinary BOpLT (NGOInteger 11) (NGOInteger 10) @?= (NGOBool False)
case_bop_lt_3 = evalBinary BOpLT (NGOInteger 10) (NGOInteger 11) @?= (NGOBool True)

case_bop_lte_1 = evalBinary BOpLTE (NGOInteger 10) (NGOInteger 10) @?= (NGOBool True)
case_bop_lte_2 = evalBinary BOpLTE (NGOInteger 11) (NGOInteger 10) @?= (NGOBool False)
case_bop_lte_3 = evalBinary BOpLTE (NGOInteger 10) (NGOInteger 11) @?= (NGOBool True)

case_bop_eq_1 = evalBinary BOpEQ (NGOInteger 10) (NGOInteger 10) @?= (NGOBool True)
case_bop_eq_2 = evalBinary BOpEQ (NGOInteger 10) (NGOInteger 0) @?= (NGOBool False)

case_bop_neq_1 = evalBinary BOpNEQ (NGOInteger 0) (NGOInteger 10) @?= (NGOBool True)
case_bop_neq_2 = evalBinary BOpNEQ (NGOInteger 10) (NGOInteger 10) @?= (NGOBool False)

case_bop_add_1 = evalBinary BOpAdd (NGOInteger 0) (NGOInteger 10) @?= (NGOInteger 10)
case_bop_add_2 = evalBinary BOpAdd (NGOInteger 10) (NGOInteger 0) @?= (NGOInteger 10)
case_bop_add_3 = evalBinary BOpAdd (NGOInteger 10) (NGOInteger 10) @?= (NGOInteger 20)

case_bop_mul_1 = evalBinary BOpMul (NGOInteger 0) (NGOInteger 10) @?= (NGOInteger 0)
case_bop_mul_2 = evalBinary BOpMul (NGOInteger 10) (NGOInteger 0) @?= (NGOInteger 0)
case_bop_mul_3 = evalBinary BOpMul (NGOInteger 10) (NGOInteger 10) @?= (NGOInteger 100)

case_uop_minus_1 = evalMinus (NGOInteger 10) @?= (NGOInteger (-10))
case_uop_minus_2 = evalMinus (NGOInteger (-10)) @?= (NGOInteger 10)

--

case_files_in_dir = do
    x <- getFilesInDir "docs"
    length x @?= 4

case_template_id = template "a/B/c/d/xpto_1.fq" @?= template "a/B/c/d/xpto_1.fq"
case_template    = template "a/B/c/d/xpto_1.fq" @?= "xpto_1"

case_parse_filename = parseFileName "/var/folders/sample_1.9168$afterQC" @?= ("/var/folders/","sample_1")

case_temp_fp      = assertNotEqual (getTempFilePath "xpto") (getTempFilePath "xpto")
case_temp_fp_comp = assertNotEqual (getTFilePathComp "xpto") (getTFilePathComp "xpto")

assertNotEqual a b = do
    a' <- a 
    b' <- b
    mapM_ removeFile [a', b'] -- a' and b' creates a file, this line removes it.
    assertBool "a' and b' should be different" (a' /= b')

case_dir_contain_formats = doesDirContainFormats "NGLess/Tests" [".hs"] >>= assertBool "doesDirContainFormats \"NGLess/Tests\" [\".hs\"]" 

-- Json Operations

case_basicInfoJson = basicInfoToJson "x1" 1.0 "x2" 2 (3,4) @?= (encode $ BasicInfo "x1" 1.0 "x2" 2 (3,4))


-- should be the same
case_createFileProcessed = do
    x <- createFilesProcessed "test" "script"
    y <- createFilesProcessed "test" "script"
    x @?= y

-- Sam operations

case_isAligned_sam = isAligned (SamLine ud 16 ud 0 0 ud ud 0 0 ud ud) @? "Should be aligned"
case_isAligned_raw = isAligned (head . readAlignments $ r) @? "Should be aligned"
    where
        r = "SRR070372.3\t16\tV\t7198336\t21\t26M3D9M3D6M6D8M2D21M\t*\t0\t0\tCCCTTATGCAGGTCTTAACACAATTCTTGTATGTTCCATCGTTCTCCAGAATGAATATCAATGATACCAA\t014<<BBBBDDFFFDDDDFHHFFD?@??DBBBB5555::?=BBBBDDF@BBFHHHHHHHFFFFFD@@@@@\tNM:i:14\tMD:Z:26^TTT9^TTC6^TTTTTT8^AA21\tAS:i:3\tXS:i:0"

case_isNotAligned = (not $ isAligned (SamLine ud 4 ud 0 0 ud ud 0 0 ud ud)) @? "Should not be aligned"

case_isUnique = isUnique (SamLine ud 16 ud 0 10 ud ud 0 0 ud ud) @? "Should be unique"
case_isNotUnique = (not $ isUnique (SamLine ud 4 ud 0 0 ud ud 0 0 ud ud)) @? "Should not be unique"
ud = undefined

case_read_one_Sam_Line = readAlignments samLineFlat @?= [samLine]
case_read_mul_Sam_Line = readAlignments (L.unlines $ replicate 10 samLineFlat) @?= replicate 10 samLine

samLineFlat = "IRIS:7:3:1046:1723#0\t4\t*\t0\t0\t*\t*\t0\t0\tAAAAAAAAAAAAAAAAAAAAAAA\taaaaaaaaaaaaaaaaaa`aa`^\tAS:i:0  XS:i:0"
samLine = SamLine {samQName = "IRIS:7:3:1046:1723#0", samFlag = 4, samRName = "*", samPos = 0, samMapq = 0, samCigar = "*", samRNext = "*", samPNext = 0, samTLen = 0, samSeq = "AAAAAAAAAAAAAAAAAAAAAAA", samQual = "aaaaaaaaaaaaaaaaaa`aa`^"}   


-- Tests with scripts (This will pass to a shell script)

--preprocess_s = "ngless '0.0'\n\
--    \input = fastq('test_samples/sample.fq')\n\
--    \preprocess(input) using |read|:\n\
--    \   read = read[3:]\n\
--    \   read = read[: len(read) ]\n\
--    \   read = substrim(read, min_quality=5)\n\
--    \   if len(read) > 20:\n\
--    \       continue\n\
--    \   if len(read) <= 20:\n\
--    \       discard\n\
--    \write(input, ofile='test_samples/sample.fq')\n"


--map_s = "ngless '0.0'\n\
--    \input = fastq('test_samples/sample.fq')\n\
--    \preprocess(input) using |read|:\n\
--    \    if len(read) < 20:\n\
--    \        discard\n\
--    \mapped = map(input,reference='sacCer3')\n\
--    \write(mapped, ofile='test_samples/sample.sam',format={sam})\n"

--case_preprocess_script = case parsetest preprocess_s >>= checktypes of
--        Left err -> T.putStrLn err
--        Right expr -> do
--            _ <- defaultDir >>= createDirIfExists  -- this is the dir where everything will be kept.
--            (interpret preprocess_s) . nglBody $ expr
--            res' <- B.readFile "test_samples/sample.fq"
--            (length $ B.lines res') @?= (16 :: Int)

--case_map_script = case parsetest map_s >>= checktypes of
--        Left err -> T.putStrLn err
--        Right expr -> do
--            _ <- defaultDir >>= createDirIfExists  -- this is the dir where everything will be kept.
--            (interpret map_s) . nglBody $ expr
--            res' <- unCompress "test_samples/sample.sam"
--            calcSamStats res' @?= [5,0,0,0]

-- Test compute stats

case_compute_stats_lc = do
    contents <- unCompress "test_samples/sample_small.fq"
    (lc $ computeStats contents) @?= ']'

-- Parse GFF lines

case_read_annotation_comp = do
    c <- unCompress "test_samples/sample.gtf.gz" 
    length (GFF.readAnnotations c) @?= 98994

case_read_annotation_uncomp = do
    c <- unCompress "test_samples/sample.gtf" 
    length (GFF.readAnnotations c) @?= 98994


gff_line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"

case_check_attr_tag_1 = GFF.checkAttrTag "id = 10;" @?= '='
case_check_attr_tag_2 = GFF.checkAttrTag "id 10;" @?= ' '

case_trim_attrs_1  = GFF.trimString " x = 10" @?= "x = 10"
case_trim_attrs_2  = GFF.trimString " x = 10 " @?= "x = 10"
case_trim_attrs_3  = GFF.trimString "x = 10 " @?= "x = 10"
case_trim_attrs_4  = GFF.trimString "x = 10" @?= "x = 10"


case_parse_gff_line = GFF.readLine gff_line @?= gff_structure
case_parse_gff_atributes = (GFF.parseGffAttributes (GFF.gffAttributes gff_structure)) @?= [("gene_id","Y74C9A.3"), ("transcript_id" ,"NM_058260"), ("gene_name", "Y74C9A.3"), ("p_id", "P23728"), ("tss_id", "TSS14501")]

-- teste parseGffAttributes
case_parse_gff_atributes_normal_1 = GFF.parseGffAttributes "ID=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("ID","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_normal_2 = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI;" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del_space = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI; " @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]

-- Setup

gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = Just (NGOList  [ NGOSymbol "gene", NGOSymbol "cds", NGOSymbol "exon"])
gff_features_gene = Just (NGOList [ NGOSymbol "gene"])
gff_features_cds = Just (NGOList  [ NGOSymbol "cds" ])

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

----

case_filter_features_1 = filter (GFF.filterFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (GFF.filterFeatures Nothing) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (GFF.filterFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (GFF.filterFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (GFF.filterFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []


case_cigar_to_length_1 = cigarTLen "18M2D19M" @?= 39
case_cigar_to_length_2 = cigarTLen "37M" @?= 37
case_cigar_to_length_3 = cigarTLen "3M1I3M1D5M" @?= 12

--- Count operation

ds_annot_gene = "x\tgene\t10\t+\n"
ds_annot_cds = "x\tCDS\t11\t+\n"
ds_annot_exon = "x\texon\t12\t+\n"
ds_annot_counts = L.concat [ds_annot_gene, ds_annot_cds, ds_annot_exon]

annot_features_gene = Just (NGOList  [ NGOSymbol "gene" ])
annot_features_cds =  Just (NGOList  [ NGOSymbol "cds"  ])
annot_features_exon = Just (NGOList  [ NGOSymbol "exon" ])

annot_features_gene_cds = Just (NGOList  [ NGOSymbol "gene", NGOSymbol "cds" ])
annot_features_cds_exon = Just (NGOList  [ NGOSymbol "exon", NGOSymbol "cds" ])

annot_features_all =  Just (NGOList  [ NGOSymbol "gene", NGOSymbol "cds", NGOSymbol "exon" ])


case_annot_count_none = filterAnnot ds_annot_counts Nothing (NGOInteger 0) @?= readAnnotCounts ds_annot_counts
case_annot_count_all = filterAnnot ds_annot_counts annot_features_all (NGOInteger 0) @?= readAnnotCounts ds_annot_counts

-- simple case. Filter all but one element
case_annot_count_gene = filterAnnot ds_annot_counts annot_features_gene (NGOInteger 0) @?= readAnnotCounts ds_annot_gene
case_annot_count_cds = filterAnnot ds_annot_counts annot_features_cds (NGOInteger 0) @?= readAnnotCounts ds_annot_cds
case_annot_count_exon = filterAnnot ds_annot_counts annot_features_exon (NGOInteger 0) @?= readAnnotCounts ds_annot_exon

-- empty case
case_annot_count_other_empty = filterAnnot ds_annot_counts (Just (NGOList  [ NGOSymbol "other" ])) (NGOInteger 0) @?= []

-- Filter all but one element
case_annot_count_gene_cds = filterAnnot ds_annot_counts annot_features_gene_cds (NGOInteger 0) @?= (readAnnotCounts $ L.concat [ds_annot_gene, ds_annot_cds])
case_annot_count_cds_exon = filterAnnot ds_annot_counts annot_features_cds_exon (NGOInteger 0) @?= (readAnnotCounts $ L.concat [ds_annot_cds, ds_annot_exon])


-- Min value of occurrences to count operation
case_annot_count_lim_no_feat = filterAnnot ds_annot_counts Nothing (NGOInteger 30) @?= []
case_annot_count_lim_feat = filterAnnot ds_annot_counts annot_features_all (NGOInteger 30) @?= []


-- interval mode 
--case_annot_interval_none = getIntervalQuery Nothing == IM.intersecting
case_interval_map_subsumes_1 = IM.subsumes (IM.ClosedInterval (1 :: Integer) 5) (IM.ClosedInterval 3 6) @?= False
case_interval_map_subsumes_2 = IM.subsumes (IM.ClosedInterval (1 :: Integer) 5) (IM.ClosedInterval 3 5) @?= True
case_interval_map_subsumes_4 = IM.subsumes (IM.ClosedInterval (3 :: Integer) 5) (IM.ClosedInterval 1 500) @?= False
case_interval_map_subsumes_3 = IM.subsumes (IM.ClosedInterval (1 :: Integer) 500) (IM.ClosedInterval 3 5) @?= True


case_interval_map_overlaps_1 = IM.overlaps  (IM.ClosedInterval (1 :: Integer) 5) (IM.ClosedInterval 6 7) @?= False
case_interval_map_overlaps_2 = IM.overlaps  (IM.ClosedInterval (3 :: Integer) 6) (IM.ClosedInterval 1 5) @?= True
case_interval_map_overlaps_3 = IM.overlaps  (IM.ClosedInterval (300 :: Integer) 400) (IM.ClosedInterval 200 300) @?= True


case_not_InsideInterval_1 = isInsideInterval 0 (IM.ClosedInterval 1 5) @?= False
case_not_InsideInterval_2 = isInsideInterval 6 (IM.ClosedInterval 1 5) @?= False

case_isInsideInterval_1 = isInsideInterval 3 (IM.ClosedInterval 1 5) @?= True
case_isInsideInterval_2 = isInsideInterval 1 (IM.ClosedInterval 1 5) @?= True
case_isInsideInterval_3 = isInsideInterval 5 (IM.ClosedInterval 3 5) @?= True


k1 = (IM.ClosedInterval 10 20, readAnnotCounts "x\tgene\t10\t+\n")
k2 = (IM.ClosedInterval 1 5,   readAnnotCounts "y\tgene\t10\t+\n")
k3 = (IM.ClosedInterval 30 30, readAnnotCounts "x\tgene\t20\t+\n")

imap1   = IM.fromList [k1]
imap2   = IM.fromList [k2]
imapAll = IM.fromList [k1, k2]

imap1Dup   = IM.fromList [k2, k2] -- same pair
imap2Dup   = IM.fromList [k1, k3] -- same pair
imap3Dup   = IM.fromList [k1, k3, k1] -- same id

-- union
case_union_empty       = union [IM.empty, IM.empty] @?= IM.empty
case_union_one_empty_1 = union [imapAll, IM.empty]   @?= imapAll
case_union_one_empty_2 = union [IM.empty, imapAll]   @?= imapAll

case_union_same  = union [imapAll, imapAll] @?= imapAll
case_union_dif   = union [imap1, imap2]     @?= imapAll

-- intersection_strict
case_intersection_strict_empty       = intersection_strict [IM.empty, IM.empty] @?= IM.empty
case_intersection_strict_one_empty_1 = intersection_strict [imapAll, IM.empty]   @?= IM.empty
case_intersection_strict_one_empty_2 = intersection_strict [IM.empty, imapAll]   @?= IM.empty

case_intersection_strict_dif      = intersection_strict [imap1, imap2] @?= IM.empty
case_intersection_strict_normal_1 = intersection_strict [imap1, imapAll] @?= imap1
case_intersection_strict_normal_2 = intersection_strict [imapAll, imap1] @?= imap1
case_intersection_strict_same     = intersection_strict [imapAll, imapAll] @?= imapAll

-- intersection_non_empty
case_intersection_nonempty_empty   = intersection_non_empty [IM.empty, IM.empty] @?= IM.empty
case_intersection_nonempty_empty_1 = intersection_non_empty [imapAll, IM.empty]   @?= imapAll
case_intersection_nonempty_empty_2 = intersection_non_empty [IM.empty, imapAll]   @?= imapAll

case_intersection_nonempty_dif      = intersection_non_empty [imap1, imap2] @?= IM.empty
case_intersection_nonempty_normal_1 = intersection_non_empty [imap1, imapAll] @?= imap1
case_intersection_nonempty_normal_2 = intersection_non_empty [imapAll, imap1] @?= imap1
case_intersection_nonempty_same     = intersection_non_empty [imapAll, imapAll] @?= imapAll


case_size_no_dup_normal = sizeNoDup imapAll @?= 2
case_size_no_dup_empty  = sizeNoDup IM.empty @?= 0

case_size_no_dup_duplicate_1 = sizeNoDup imap1Dup @?= 1
case_size_no_dup_duplicate_2 = sizeNoDup imap2Dup @?= 1
case_size_no_dup_duplicate_3 = sizeNoDup imap3Dup @?= 1



----- VectorOperations.hs
case_zero_vec = do
  v <- zeroVec 4 >>= V.freeze
  v @?= V.fromList [0,0,0,0]

----- SamBamOperations.hs

case_sam_stats_length = do
    contents <- unCompress "test_samples/sample.sam"
    V.length (samStats contents) @?= 4

case_sam_stats_res = do
    contents <- unCompress "test_samples/sample.sam"
    samStats contents @?= V.fromList  [3072,1610,1554,0]

case_calc_sam_stats = do
  r <- unCompress "test_samples/sample.sam" >>= return . calcSamStats
  r @?=  [3072,1610,1554,0]

--- Unique.hs

--File "test_samples/data_set_repeated.fq" has 216 reads in which 54 are unique. 

case_num_files_1 = do
  n <- numFiles "test_samples/data_set_repeated.fq" 
  n @?= 1

case_num_files_2 = do -- github rejects files with more than 100MB
  n <- numFiles "test_samples/sample.sam" 
  n @?= 1

case_unique_1_read = do
    c <- readReadSet enc "test_samples/data_set_repeated.fq" 
    p <- writeToNFiles "test_samples/data_set_repeated.fq" enc c
    ds <- readNFiles enc 1  p
    removeDirectoryRecursive p -- need to do this by hand to emulate normal execution. 
    length ds @?=  54
  where enc = 64

case_unique_2_read = do
    c <- readReadSet enc "test_samples/data_set_repeated.fq" 
    p <- writeToNFiles "test_samples/data_set_repeated.fq" enc c
    ds <- readNFiles enc 2 p 
    removeDirectoryRecursive p -- need to do this by hand to emulate normal execution. 
    length ds @?=  (2 * 54)
  where enc = 64

case_unique_3_read = do
    c <- readReadSet enc "test_samples/data_set_repeated.fq" 
    p <- writeToNFiles "test_samples/data_set_repeated.fq" enc c
    ds <- readNFiles enc 3 p 
    removeDirectoryRecursive p -- need to do this by hand to emulate normal execution. 
    length ds @?=  (3 * 54)
  where enc = 64

case_unique_4_read = do
    c <- readReadSet enc "test_samples/data_set_repeated.fq" 
    p <- writeToNFiles "test_samples/data_set_repeated.fq" enc c
    ds <- readNFiles enc 4 p 
    removeDirectoryRecursive p -- need to do this by hand to emulate normal execution.     
    length ds @?=  (4 * 54)
  where enc = 64

case_unique_5_read = do
    c <- readReadSet enc "test_samples/data_set_repeated.fq" 
    p <- writeToNFiles "test_samples/data_set_repeated.fq" enc c
    ds <- readNFiles enc 5 p 
    removeDirectoryRecursive p -- need to do this by hand to emulate normal execution.     
    length ds @?=  (4 * 54)
  where enc = 64

-- PerBaseQualityScores 

case_calc_perc_med = _calcPercentile bps eT percentile50 @?= 4
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 21 -> mul: 0,5  +- 11 in arr = 13 index 4

case_calc_perc_lq = _calcPercentile bps eT lowerQuartile @?= 2
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 21 -> mul: 0,25 -> 6 in arr = 6 index 2

case_calc_perc_uq = _calcPercentile bps eT upperQuartile @?= 5
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 8 -> mul: 0,75 -> 16 in arr = 18 index 5


-- negative tests quality on value 60 char ';'. Value will be 60 - 64 which is -4
case_calc_statistics_negative = do
    s <- unCompress "test_samples/sample_low_qual.fq" >>= return . computeStats
    head (stats' s) @?= (-4,-4,-4,-4)
  where stats' s = calculateStatistics (qualCounts s) (ord . lc $ s)

-- low positive tests quality on 65 char 'A'. Value will be 65-64 which is 1.
case_calc_statistics_low_positive = do
    s <- unCompress "test_samples/sample_low_qual.fq" >>= return . computeStats
    last (stats' s) @?= (1,1,1,1)
  where stats' s = calculateStatistics (qualCounts s) (ord . lc $ s)


case_calc_statistics_normal = do
    s <- unCompress "test_samples/data_set_repeated.fq" >>= return . computeStats
    head (stats' s) @?= (25,33,31,33)
  where stats' s = calculateStatistics (qualCounts s) (ord . lc $ s)

case_json_statistics = do
    s <- unCompress "test_samples/sample_small.fq" >>= return . computeStats
    r <- unCompress "test_samples/res_json_statistics.txt" >>= return . L.unpack
    createDataString (stats' s) @?= r
  where stats' s = calculateStatistics (qualCounts s) (ord . lc $ s)

-- Annotate

ngo_gff_fp = (Just (NGOString "test_samples/sample.gtf"))

case_test_setup_html_view = do
    _ <- setupHtmlViewer "Html/"  -- Make sure tmp has the required files, but use source to populate it.
    dst <- defaultDir
    doesFileExist (p' dst) >>= \x -> x @?= True -- make sure keeper.html exist
  where 
    p' = (</> "nglessKeeper.html")


all_default_annot = do
  annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing Nothing Nothing Nothing >>= unCompress . T.unpack

-- test default values
case_annotate_features_default_idemp = do
    a1 <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing Nothing Nothing Nothing 
    res1 <- unCompress $ T.unpack a1
    res2 <- all_default_annot
    res1 @?= res2
  where feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_mode_default_idemp = do
    a1 <- annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing s Nothing Nothing
    res1 <- unCompress $ T.unpack a1
    res2 <- all_default_annot
    res1 @?= res2
  where s = Just $ NGOSymbol "union"


case_annotate_amb_default_idemp = do
    a1 <- annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing Nothing amb Nothing
    res1 <- unCompress $ T.unpack a1
    res2 <- all_default_annot
    res1 @?= res2
  where amb = Just $ NGOSymbol "allow"

case_annotate_strand_default_idemp = do
    a1 <- annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing Nothing Nothing s
    res1 <- unCompress $ T.unpack a1
    res2 <- all_default_annot
    res1 @?= res2
  where s = Just $ NGOSymbol "no"


case_annotate_gene_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        m = Just $ NGOSymbol "union"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "no"


case_annotate_exon_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "exon"]
        m = Just $ NGOSymbol "union"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "no"


case_annotate_cds_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "CDS"]
        m = Just $ NGOSymbol "union"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "no"


case_annotate_gene_noStrand_inters_strict = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        m = Just $ NGOSymbol "intersection_strict"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "no"


case_annotate_gene_noStrand_inters_non_empty = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        m = Just $ NGOSymbol "intersection_non_empty"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "no"



case_annotate_gene_yesStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing m amb s
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- unCompress $ T.unpack p
    resHT <- unCompress $ "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        m = Just $ NGOSymbol "union"
        amb = Just $ NGOSymbol "deny" --htseq does not allow ambiguity.
        s = Just $ NGOSymbol "yes"

-- MapOperations

-- install genome User mode

case_install_genome_user_mode = do
  r1 <- configGenome "ce10" User
  p <- userDataDirectory  >>= return . (</> (getIndexPath "ce10"))
  r1 @?= p 


-- ProcessFastQ
low_char_int = do
  unCompress "test_samples/sample.fq" >>= return . ord . lc . computeStats

case_read_and_write_fastQ = do
    enc <- low_char_int >>= return . offset . calculateEncoding
    rs <- readReadSet enc "test_samples/sample.fq"
    fp <- writeReadSet "test_samples/sample.fq" rs enc
    newrs <- readReadSet enc $ B.pack fp
    newrs @?= rs

-- hack: jump over copy of .html and .css
case_read_fastQ = do
    nt <- generateDirId fp 
    createDirIfNotExists (dstDir nt)
    len <- readFastQ Nothing fp (dstDir nt) nt >> getFilesInDir (dstDir nt) >>= return . length --populates dir nt
    removeDirectoryRecursive $ dstDir nt -- delete test generated data.
    len @?= 2
  where fp = "test_samples/sample.fq"
        dstDir nt = nt ++ "$beforeQC"

-- "test_samples/sample.fq" has 33 as lowest char from the initial data set
case_read_fastQ_store_enc = do
    nt <- generateDirId fp 
    createDirIfNotExists $ dstDirBef nt
    createDirIfNotExists $ dstDirAft nt
    (NGOReadSet _ eb _) <- readFastQ Nothing   fp (dstDirBef nt) nt
    (NGOReadSet _ ea _) <- readFastQ (Just eb) fp (dstDirAft nt) nt
    removeDirectoryRecursive $ dstDirBef nt -- delete test generated data.
    removeDirectoryRecursive $ dstDirAft nt -- delete test generated data.
    eb @?= ea
  where fp = "test_samples/sample.fq"
        dstDirBef = (++ "$beforeQC")
        dstDirAft = (++ "$afterQC")

case_get_gff = getGff "path_to_gff" @?= "path_to_gff/Annotation/annot.gtf.gz"

