{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- Unit tests are their own programme.

module Main where

import Test.Framework
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Text.Parsec (parse)
import Text.Parsec.Combinator (eof)

import System.Directory (removeFile
                        ,removeDirectoryRecursive
                        ,doesFileExist
                        )
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit ((=$=), ($$))


import Data.Convertible

import Language
import Interpret
import Tokens
import Types
import FileManagement
import Configuration
import NGLess

import Interpretation.Map
import Interpretation.Unique

import Data.FastQ
import Utils.Conduit
import Utils.Here
import qualified Data.GFF as GFF

import Tests.Utils
import Tests.FastQ
import Tests.Validation
import Tests.Select (tgroup_Select)
import Tests.Types (tgroup_Types)
import Tests.Count (tgroup_Count)
import Tests.Parse (tgroup_Parse)
import Tests.Vector (tgroup_Vector)
import Tests.IntGroups (tgroup_IntGroups)
import Tests.NGLessAPI (tgroup_NGLessAPI)

test_FastQ      = [tgroup_FastQ]
test_Validation = [tgroup_Validation]
test_Count      = [tgroup_Count]
test_Parse      = [tgroup_Parse]
test_Types      = [tgroup_Types]
test_NGLessAPI  = [tgroup_NGLessAPI]
test_Vector     = [tgroup_Vector]
test_IntGroups  = [tgroup_IntGroups]
test_Select  = [tgroup_Select]

-- The main test driver sets up the config options then uses the automatically
-- generated function
main = do
    setupTestConfiguration
    $(defaultMainGenerator)
    removeDirectoryRecursive "testing_directory_tmp"

-- Test Tokens module
tokenize' fn t = map snd <$> (tokenize fn t)

case_tok_cr = TNewLine @=? (case parse (_eol <* eof) "test" "\r\n" of { Right t -> t; Left _ -> error "Parse failed"; })
case_tok_single_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0# comment\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstInt 0),TNewLine,TWord "b",TOperator '=',TExpr (ConstInt 1),TNewLine]

case_tok_single_line_comment_cstyle = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0// comment\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstInt 0),TNewLine,TWord "b",TOperator '=',TExpr (ConstInt 1),TNewLine]

case_tok_multi_line_comment = tokenize' "test" with_comment @?= Right expected
    where
        with_comment = "a=0/* This\n\nwith\nlines*/\nb=1\n"
        expected = [TWord "a",TOperator '=',TExpr (ConstInt 0),TIndent 0,TNewLine,TWord "b",TOperator '=',TExpr (ConstInt 1),TNewLine]

case_tok_word_ = tokenize' "test" "word_with_underscore" @?= Right expected
    where
        expected = [TWord "word_with_underscore"]



-- Test Types
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


-- Type Validate pre process operations
sr i s q = NGOShortRead (ShortRead i s $ VU.generate (B.length q) (convert . B.index q))

case_pre_process_indexation_1 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 5), Nothing] @?= (sr "@IRIS" "CAA" "aaa")
case_pre_process_indexation_2 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Nothing, Just (NGOInteger 3)] @?= (sr "@IRIS" "AGT" "aa`")
case_pre_process_indexation_3 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 2), Just (NGOInteger 5)] @?= (sr "@IRIS" "TAC" "`aa")

_evalIndex' a b = case _evalIndex a b of
    Right v -> v
    Left err -> error (show err)

case_pre_process_length_1 = _evalUnary UOpLen (sr "@IRIS" "AGTACCAA" "aa`aaaaa") @?= Right (NGOInteger 8)

case_bop_gte_1 = _evalBinary BOpGTE (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gte_2 = _evalBinary BOpGTE (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gte_3 = _evalBinary BOpGTE (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool False)

case_bop_gt_1 = _evalBinary BOpGT (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_gt_2 = _evalBinary BOpGT (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gt_3 = _evalBinary BOpGT (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool False)

case_bop_lt_1 = _evalBinary BOpLT (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lt_2 = _evalBinary BOpLT (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lt_3 = _evalBinary BOpLT (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool True)

case_bop_lte_1 = _evalBinary BOpLTE (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_lte_2 = _evalBinary BOpLTE (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lte_3 = _evalBinary BOpLTE (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool True)

case_bop_eq_1 = _evalBinary BOpEQ (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_eq_2 = _evalBinary BOpEQ (NGOInteger 10) (NGOInteger 0) @?= Right (NGOBool False)

case_bop_neq_1 = _evalBinary BOpNEQ (NGOInteger 0) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_neq_2 = _evalBinary BOpNEQ (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)

case_bop_add_1 = _evalBinary BOpAdd (NGOInteger 0) (NGOInteger 10) @?= Right (NGOInteger 10)
case_bop_add_2 = _evalBinary BOpAdd (NGOInteger 10) (NGOInteger 0) @?= Right (NGOInteger 10)
case_bop_add_3 = _evalBinary BOpAdd (NGOInteger 10) (NGOInteger 10) @?= Right (NGOInteger 20)

case_bop_mul_1 = _evalBinary BOpMul (NGOInteger 0) (NGOInteger 10) @?= Right (NGOInteger 0)
case_bop_mul_2 = _evalBinary BOpMul (NGOInteger 10) (NGOInteger 0) @?= Right (NGOInteger 0)
case_bop_mul_3 = _evalBinary BOpMul (NGOInteger 10) (NGOInteger 10) @?= Right (NGOInteger 100)

case_uop_minus_1 = _evalUnary UOpMinus (NGOInteger 10) @?= Right (NGOInteger (-10))
case_uop_minus_2 = _evalUnary UOpMinus (NGOInteger (-10)) @?= Right (NGOInteger 10)

--

case_template_id = takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq" @?= takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq"
case_template    = takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq" @?= "xpto_1"


preprocess_s = "ngless '0.0'\n\
    \input = fastq('test_samples/sample20.fq')\n\
    \preprocess(input) using |read|:\n\
    \   read = read[3:]\n\
    \   read = read[: len(read) ]\n\
    \   read = substrim(read, min_quality=5)\n\
    \   if len(read) > 20:\n\
    \       continue\n\
    \   if len(read) <= 20:\n\
    \       discard\n\
    \write(input, ofile='test_samples/sample20_post.fq')\n"


case_preprocess_script = case parsetest preprocess_s >>= checktypes [] of
    Left err -> assertFailure (show err)
    Right expr -> do
        testNGLessIO $ (interpret []) . nglBody $ expr
        res' <- B.readFile "test_samples/sample20_post.fq"
        (length $ B8.lines res') @?= (16 :: Int)
        removeFile "test_samples/sample20_post.fq"

case_sam20 = do
        sam <- testNGLessIO $ asTempFile sam20 "sam"  >>= _samStats
        sam @?= (5,0,0)
    where
        sam20 = [here|
@SQ	SN:I	LN:230218
@PG	ID:bwa	PN:bwa	VN:0.7.7-r441	CL:/home/luispedro/.local/share/ngless/bin/ngless-0.0.0-bwa mem -t 1 /home/luispedro/.local/share/ngless/data/sacCer3/Sequence/BWAIndex/reference.fa.gz /tmp/preprocessed_sample20.fq1804289383846930886.gz
IRIS:7:1:17:394#0	4	*	0	0	*	*	0	0	GTCAGGACAAGAAAGACAANTCCAATTNACATT	aaabaa`]baaaaa_aab]D^^`b`aYDW]aba	AS:i:0	XS:i:0
IRIS:7:1:17:800#0	4	*	0	0	*	*	0	0	GGAAACACTACTTAGGCTTATAAGATCNGGTTGCGG	ababbaaabaaaaa`]`ba`]`aaaaYD\\_a``XT	AS:i:0	XS:i:0
IRIS:7:1:17:1757#0	4	*	0	0	*	*	0	0	TTTTCTCGACGATTTCCACTCCTGGTCNAC	aaaaaa``aaa`aaaa_^a```]][Z[DY^	AS:i:0	XS:i:0
IRIS:7:1:17:1479#0	4	*	0	0	*	*	0	0	CATATTGTAGGGTGGATCTCGAAAGATATGAAAGAT	abaaaaa`a```^aaaaa`_]aaa`aaa__a_X]``	AS:i:0	XS:i:0
IRIS:7:1:17:150#0	4	*	0	0	*	*	0	0	TGATGTACTATGCATATGAACTTGTATGCAAAGTGG	abaabaa`aaaaaaa^ba_]]aaa^aaaaa_^][aa	AS:i:0	XS:i:0
|]
-- Parse GFF lines

gff_line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_attributes = "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure  = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "Y74C9A.3"


case_trim_attrs_1  = GFF._trimString " x = 10" @?= "x = 10"
case_trim_attrs_2  = GFF._trimString " x = 10 " @?= "x = 10"
case_trim_attrs_3  = GFF._trimString "x = 10 " @?= "x = 10"
case_trim_attrs_4  = GFF._trimString "x = 10" @?= "x = 10"
case_trim_attrs_5  = GFF._trimString "   X    " @?= "X"


case_parse_gff_line = GFF.readGffLine gff_line @?= Right gff_structure
case_parse_gff_atributes = GFF._parseGffAttributes gff_attributes @?= [("gene_id","Y74C9A.3"), ("transcript_id" ,"NM_058260"), ("gene_name", "Y74C9A.3"), ("p_id", "P23728"), ("tss_id", "TSS14501")]

-- _parseGffAttributes
case_parse_gff_atributes_normal_1 = GFF._parseGffAttributes "ID=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("ID","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_normal_2 = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI;" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del_space = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI; " @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]


case_calc_sam_stats = testNGLessIO (_samStats "test_samples/sample.sam.gz") >>= \r ->
  r @?=  (2772,1310,1299)

--- Unique.hs

--File "test_samples/data_set_repeated.fq" has 216 reads in which 54 are unique.

countC = loop (0 :: Int)
    where
        loop !n = C.await >>= maybe (return n) (const (loop $ n+1))
make_unique_test n = let enc = SolexaEncoding in do
    nuniq <- testNGLessIO $ do
        newfp <- performUnique "test_samples/data_set_repeated.fq" enc n
        conduitPossiblyCompressedFile newfp
                =$= linesC
                =$= fqDecodeC enc
                $$ countC
    let n' = min n 4
    nuniq @?=  (n' * 54)

case_unique_1 = make_unique_test 1
case_unique_2 = make_unique_test 2
case_unique_3 = make_unique_test 3
case_unique_4 = make_unique_test 4
case_unique_5 = make_unique_test 5

case_test_setup_html_view = do
    setupHtmlViewer "testing_tmp_dir_html"
    ex <- doesFileExist "testing_tmp_dir_html/index.html"
    assertBool "index.html should be present after setupHtmlViewer" ex
    removeDirectoryRecursive "testing_tmp_dir_html/"

case_async_gzip_to_from = do
    let testdata = [0 :: Int .. 12]
    result <- testNGLessIO $ do
        CL.sourceList testdata
            =$= CL.map (B8.pack . (\n -> show n ++ "\n"))
            $$ asyncGzipToFile "testing_tmp_dir/test.gz"
        asyncGzipFromFile "testing_tmp_dir/test.gz" $$ asyncGzipToFile "testing_tmp_dir/test-copied.gz"
        asyncGzipFromFile "testing_tmp_dir/test-copied.gz"
            =$= linesC
            =$= CL.map (read . B8.unpack .  unwrapByteLine)
            $$ CL.consume
    result @?= testdata

case_async_gzip_from_not_gzip = do
    -- this is a regression test.
    _ <- testNGLessIO $ (asyncGzipFromFile "Makefile" $$ CL.consume)
    return ()


