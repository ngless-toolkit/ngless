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

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.Text as T
import qualified Data.Text.IO as T


import Language
import Interpret
import Parse
import Tokens
import Types
import PrintFastqBasicStats
import PerBaseQualityScores
import FPreProcess
import FileManagement
import MapInterpretOperation
import Validation

import Data.Sam
import Data.Json
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
case_calculateEncoding_illumina_1 = calculateEncoding 60 @?= Encoding "Illumina <1.3" illumina_1_encoding_offset
case_calculateEncoding_illumina_1_5 = calculateEncoding 100 @?= Encoding "Illumina 1.5" illumina_1_3_encoding_offset

--Test the calculation of the Mean
case_calc_simple_mean = calcMean (500 :: Int) (10 :: Int) @?= (50 :: Double) 

--- SETUP to reduce imports.
-- test array: "\n\v\f{zo\n\v\NUL" -> [10,11,12,123,122,111,10,11,0]
-- test cutoff: chr 20 -> '\DC4'

--Property 1: For every s, the size must be allways smaller than the input
prop_substrim_maxsize s = st >= 0 && e <= B.length (B.pack s)
    where (st,e) = calculateSubStrim (B.pack s) '\DC4'

-- Property 2: substrim should be idempotent
prop_substrim_idempotent s = st == 0 && e == B.length s1
    where
        s1 = removeBps (B.pack s) (calculateSubStrim (B.pack s) '\DC4')
        (st,e) = calculateSubStrim s1 '\DC4'
                        
case_substrim_normal_exec =  calculateSubStrim "\n\v\f{zo\n\v\NUL" '\DC4' @?= (3,3)
case_substrim_empty_quals = calculateSubStrim "" '\DC4' @?= (0,0)

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
    \reads = unique(reads,maxCopies=2)\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    read = substrim(read, minQuality=24)\n\
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

-----
--- Validation pure functions

case_bad_function_attr_count = isError $ parsetest function_attr >>= validate
    where function_attr = "ngless '0.0'\n\
            \count(annotated, count={gene})\n"

case_good_function_attr_count_1 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \write(count(annotated, count={gene}),ofile='gene_counts.csv',format={csv})"

case_good_function_attr_count_2 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \counts = count(annotated, count={gene})"

case_bad_function_attr_map = isError $ parsetest function_attr >>= validate
    where function_attr = "ngless '0.0'\n\
            \map(input,reference='sacCer3')\n"

case_good_function_attr_map_1 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \write(map(input,reference='sacCer3'),ofile='result.sam',format={sam})"

case_good_function_attr_map_2 = isOkTypes $ parsetest good_function_attr >>= validate
    where good_function_attr = "ngless '0.0'\n\
            \counts = map(input,reference='sacCer3')"

-----
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
    length x @?= 2

case_parse_filename = parseFileName "/var/folders/sample_1.9168$afterQC" @?= ("/var/folders/","sample_1")


-- Json Operations

case_basicInfoJson = basicInfoToJson "x1" 1.0 "x2" 2 (3,4) @?= "{\"GC\":1,\"SeqLength\":[3,4],\"fileName\":\"x1\",\"Encoding\":\"x2\",\"NumSeqs\":2}" 

-- should be the same
case_createFileProcessed = do
    x <- createFilesProcessed "test" (T.pack "script")
    y <- createFilesProcessed "test" (T.pack "script")
    x @?= y

-- Sam operations

case_isAligned = isAligned (SamLine ud 16 ud 0 0 ud ud 0 0 ud ud) @? "Should be aligned"
case_isNotAligned = (not $ isAligned (SamLine ud 4 ud 0 0 ud ud 0 0 ud ud)) @? "Should not be aligned"

case_isUnique = isUnique (SamLine ud 16 ud 0 10 ud ud 0 0 ud ud) @? "Should be unique"
case_isNotUnique = (not $ isUnique (SamLine ud 4 ud 0 0 ud ud 0 0 ud ud)) @? "Should not be unique"
ud = undefined

case_read_one_Sam_Line = readAlignments samLineFlat @?= [samLine]
case_read_mul_Sam_Line = readAlignments (L.unlines $ replicate 10 samLineFlat) @?= replicate 10 samLine

samLineFlat = "IRIS:7:3:1046:1723#0\t4\t*\t0\t0\t*\t*\t0\t0\tAAAAAAAAAAAAAAAAAAAAAAA\taaaaaaaaaaaaaaaaaa`aa`^\tAS:i:0  XS:i:0"
samLine = SamLine {samQName = "IRIS:7:3:1046:1723#0", samFlag = 4, samRName = "*", samPos = 0, samMapq = 0, samCigar = "*", samRNext = "*", samPNext = 0, samTLen = 0, samSeq = "AAAAAAAAAAAAAAAAAAAAAAA", samQual = "aaaaaaaaaaaaaaaaaa`aa`^"}   


-- Tests with scripts

preprocess_s = "ngless '0.0'\n\
    \input = fastq('samples/sample.fq')\n\
    \preprocess(input) using |read|:\n\
    \   read = read[3:]\n\
    \   read = read[: len(read) ]\n\
    \   read = substrim(read, min_quality=26)\n\
    \   if len(read) > 20:\n\
    \       continue\n\
    \   if len(read) <= 20:\n\
    \       discard\n\
    \write(input, ofile='samples/resultSampleFiltered.txt')\n"


map_s = "ngless '0.0'\n\
    \input = fastq('samples/sample.fq')\n\
    \preprocess(input) using |read|:\n\
    \    if len(read) < 20:\n\
    \        discard\n\
    \mapped = map(input,reference='sacCer3')\n\
    \write(mapped, ofile='samples/resultSampleSam.sam',format={sam})\n"

case_preprocess_script = case parsetest preprocess_s >>= checktypes of
        Left err -> T.putStrLn err
        Right expr -> do
            _ <- defaultDir >>= createDirIfExists  -- this is the dir where everything will be kept.
            (interpret preprocess_s) . nglBody $ expr
            res' <- B.readFile "samples/resultSampleFiltered.txt"
            (length $ B.lines res') @?= (16 :: Int)

case_map_script = case parsetest map_s >>= checktypes of
        Left err -> T.putStrLn err
        Right expr -> do
            _ <- defaultDir >>= createDirIfExists  -- this is the dir where everything will be kept.
            (interpret map_s) . nglBody $ expr
            res' <- unCompress "samples/resultSampleSam.sam"
            calcSamStats res' @?= [5,0,0]


-- Parse GFF lines

gff_line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"

case_check_attr_tag_1 = GFF.checkAttrTag "id = 10;" @?= '='
case_check_attr_tag_2 = GFF.checkAttrTag "id 10;" @?= ' '

case_trim_attrs_1  = GFF.trimString " x = 10" @?= "x = 10"
case_trim_attrs_2  = GFF.trimString " x = 10 " @?= "x = 10"
case_trim_attrs_3  = GFF.trimString "x = 10 " @?= "x = 10"
case_trim_attrs_4  = GFF.trimString "x = 10" @?= "x = 10"


case_parse_gff_line = GFF.readLine gff_line @?= gff_structure
case_parse_gff_atributes = (GFF.parseGffAttributes (GFF.gffAttributes gff_structure)) @?= [("gene_id","\"Y74C9A.3\""), ("transcript_id" ,"\"NM_058260\""), ("gene_name", "\"Y74C9A.3\""), ("p_id", "\"P23728\""), ("tss_id", "\"TSS14501\"")]


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
case_filter_features_2 = filter (GFF.filterFeatures Nothing) gff_lines_ex @?= gff_lines_ex
case_filter_features_3 = filter (GFF.filterFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (GFF.filterFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (GFF.filterFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []


case_cigar_to_length_1 = cigarTLen "18M2D19M" @?= 39
case_cigar_to_length_2 = cigarTLen "37M" @?= 37
case_cigar_to_length_3 = cigarTLen "3M1I3M1D5M" @?= 12
