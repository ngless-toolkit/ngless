{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- | Unit tests are their own programme.
--
-- Unit tests written in Haskell have less overhead than full integration tests
-- in the tests/ directory, but are not always as convenient.

module Main where

import Test.Framework
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Text.Parsec (parse)
import Text.Parsec.Combinator (eof)

import System.Directory (removeDirectoryRecursive)
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Char8 as B

import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import           Control.Monad.State.Strict (execState, modify')
import           Data.Convertible (convert)
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)

import Language
import Interpret
import Tokens
import FileManagement
import NGLess
import NGLess.NGLEnvironment (setupTestEnvironment)

import Interpretation.Unique

import Data.Sam
import Data.FastQ
import Utils.Conduit
import Utils.Samtools (samBamConduit)
import Utils.Here
import qualified Data.GFF as GFF

import Tests.Utils
import Tests.Count (tgroup_Count)
import Tests.FastQ (tgroup_FastQ)
import Tests.IntGroups (tgroup_IntGroups)
import Tests.Language (tgroup_Language)
import Tests.LoadFQDirectory (tgroup_LoadFQDirectory)
import Tests.NGLessAPI (tgroup_NGLessAPI)
import Tests.Parse (tgroup_Parse)
import Tests.Select (tgroup_Select)
import Tests.Types (tgroup_Types)
import Tests.Validation (tgroup_Validation)
import Tests.Vector (tgroup_Vector)
import Tests.Write (tgroup_Write)

test_FastQ      = [tgroup_FastQ]
test_Validation = [tgroup_Validation]
test_Count      = [tgroup_Count]
test_Parse      = [tgroup_Parse]
test_Types      = [tgroup_Types]
test_NGLessAPI  = [tgroup_NGLessAPI]
test_Vector     = [tgroup_Vector]
test_IntGroups  = [tgroup_IntGroups]
test_Select     = [tgroup_Select]
test_Language   = [tgroup_Language]
test_LoadFqDir  = [tgroup_LoadFQDirectory]
test_Write      = [tgroup_Write]

-- The main test driver sets up the config options then uses the automatically
-- generated function
main = do
    setupTestEnvironment
    $(defaultMainGenerator)
    removeDirectoryRecursive "testing_directory_tmp"

-- Test Tokens module
tokenize' fn t = map snd <$> (tokenize fn t)

case_tok_cr = TNewLine @=? (case parse (Tokens.eol <* eof) "test" "\r\n" of { Right t -> t; Left _ -> error "Parse failed"; })
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
    \reads = preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    # comment \n"

indent_space  = "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \reads = preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    \n"


-- Type Validate pre process operations
sr i s q = NGOShortRead (ShortRead i s $ VS.generate (B.length q) (convert . B.index q))

case_pre_process_indexation_1 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 5), Nothing] @?= (sr "@IRIS" "CAA" "aaa")
case_pre_process_indexation_2 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Nothing, Just (NGOInteger 3)] @?= (sr "@IRIS" "AGT" "aa`")
case_pre_process_indexation_3 = _evalIndex' (sr "@IRIS" "AGTACCAA" "aa`aaaaa") [Just (NGOInteger 2), Just (NGOInteger 5)] @?= (sr "@IRIS" "TAC" "`aa")

_evalIndex' a b = case _evalIndex a b of
    Right v -> v
    Left err -> error (show err)

case_pre_process_length_1 = _evalUnary UOpLen (sr "@IRIS" "AGTACCAA" "aa`aaaaa") @?= Right (NGOInteger 8)

case_bop_gte_1 = evalBinary BOpGTE (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gte_2 = evalBinary BOpGTE (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gte_3 = evalBinary BOpGTE (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool False)

case_bop_gt_1 = evalBinary BOpGT (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_gt_2 = evalBinary BOpGT (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_gt_3 = evalBinary BOpGT (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool False)

case_bop_lt_1 = evalBinary BOpLT (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lt_2 = evalBinary BOpLT (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lt_3 = evalBinary BOpLT (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool True)

case_bop_lte_1 = evalBinary BOpLTE (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_lte_2 = evalBinary BOpLTE (NGOInteger 11) (NGOInteger 10) @?= Right (NGOBool False)
case_bop_lte_3 = evalBinary BOpLTE (NGOInteger 10) (NGOInteger 11) @?= Right (NGOBool True)

case_bop_eq_1 = evalBinary BOpEQ (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_eq_2 = evalBinary BOpEQ (NGOInteger 10) (NGOInteger 0) @?= Right (NGOBool False)

case_bop_neq_1 = evalBinary BOpNEQ (NGOInteger 0) (NGOInteger 10) @?= Right (NGOBool True)
case_bop_neq_2 = evalBinary BOpNEQ (NGOInteger 10) (NGOInteger 10) @?= Right (NGOBool False)

case_bop_add_1 = evalBinary BOpAdd (NGOInteger 0) (NGOInteger 10) @?= Right (NGOInteger 10)
case_bop_add_2 = evalBinary BOpAdd (NGOInteger 10) (NGOInteger 0) @?= Right (NGOInteger 10)
case_bop_add_3 = evalBinary BOpAdd (NGOInteger 10) (NGOInteger 10) @?= Right (NGOInteger 20)

case_bop_mul_1 = evalBinary BOpMul (NGOInteger 0) (NGOInteger 10) @?= Right (NGOInteger 0)
case_bop_mul_2 = evalBinary BOpMul (NGOInteger 10) (NGOInteger 0) @?= Right (NGOInteger 0)
case_bop_mul_3 = evalBinary BOpMul (NGOInteger 10) (NGOInteger 10) @?= Right (NGOInteger 100)

case_bop_add_path_1 = evalBinary BOpPathAppend (NGOString "dir") (NGOString "file") @?= Right (NGOString "dir/file")
case_bop_add_path_2 = evalBinary BOpPathAppend (NGOString "dir/subdir") (NGOString "file") @?= Right (NGOString "dir/subdir/file")
case_bop_add_path_3 = evalBinary BOpPathAppend (NGOString "dir/subdir/") (NGOString "file") @?= Right (NGOString "dir/subdir/file")
case_bop_add_path_4 = evalBinary BOpPathAppend (NGOString "../dir/subdir/") (NGOString "file") @?= Right (NGOString "../dir/subdir/file")
case_bop_add_path_5 = evalBinary BOpPathAppend (NGOString "/abs/dir/subdir/") (NGOString "file") @?= Right (NGOString "/abs/dir/subdir/file")

case_uop_minus_1 = _evalUnary UOpMinus (NGOInteger 10) @?= Right (NGOInteger (-10))
case_uop_minus_2 = _evalUnary UOpMinus (NGOInteger (-10)) @?= Right (NGOInteger 10)

--

case_template_id = takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq" @?= takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq"
case_template    = takeBaseNameNoExtensions "a/B/c/d/xpto_1.fq" @?= "xpto_1"

samStats :: FilePath -> NGLessIO (Int, Int, Int)
samStats fname = C.runConduit (samBamConduit fname .| linesVC 1024 .| samStatsC) >>= runNGLess

case_sam20 = do
        sam <- testNGLessIO $ asTempFile sam20 "sam"  >>= samStats
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



case_trim_attrs_1  = GFF._trimString " x = 10" @?= "x = 10"
case_trim_attrs_2  = GFF._trimString " x = 10 " @?= "x = 10"
case_trim_attrs_3  = GFF._trimString "x = 10 " @?= "x = 10"
case_trim_attrs_4  = GFF._trimString "x = 10" @?= "x = 10"
case_trim_attrs_5  = GFF._trimString "   X    " @?= "X"


case_parse_gff_line = GFF.readGffLine gff_line @?= Right gff_structure
    where
        gff_line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
        gff_structure  = GFF.GffLine "chrI" "unknown" "exon" 4124 4358 Nothing GFF.GffNegStrand (-1) attrsExpected
        attrsExpected = [("gene_id","Y74C9A.3"), ("transcript_id" ,"NM_058260"), ("gene_name", "Y74C9A.3"), ("p_id", "P23728"), ("tss_id", "TSS14501")]

-- _parseGffAttributes
case_parse_gff_atributes_normal_1 = GFF._parseGffAttributes "ID=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("ID","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_normal_2 = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI;" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del_space = GFF._parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI; " @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]


case_calc_sam_stats = testNGLessIO (samStats "test_samples/sample.sam.gz") >>= \r ->
  r @?=  (2772,1310,1299)

--- Unique.hs

--File "test_samples/data_set_repeated.fq" has 216 reads in which 54 are unique.

countC = loop (0 :: Int)
    where
        loop !n = C.await >>= maybe (return n) (const (loop $ n+1))
make_unique_test n = let enc = SolexaEncoding in do
    nuniq <- testNGLessIO $ do
        newfp <- performUnique "test_samples/data_set_repeated.fq" enc n
        C.runConduit $
            conduitPossiblyCompressedFile newfp
                .| linesC
                .| fqDecodeC enc
                .| countC
    let n' = min n 4
    nuniq @?=  (n' * 54)

case_unique_1 = make_unique_test 1
case_unique_2 = make_unique_test 2
case_unique_3 = make_unique_test 3
case_unique_4 = make_unique_test 4
case_unique_5 = make_unique_test 5


case_recursiveAnalyze = execState (recursiveAnalyse countFcalls expr) 0 @?= (1 :: Int)
    where
        countFcalls (FunctionCall  _ _ _ _) = modify' (+1)
        countFcalls _ = return ()

        expr = Assignment
                    (Variable "varname")
                    (FunctionCall (FuncName "count")
                        (Lookup Nothing (Variable "mapped"))
                        [(Variable "features", ListExpression [ConstStr "seqname"])
                            ,(Variable "multiple", ConstSymbol "all1")]
                        Nothing)


case_expand_path = do
    expandPath' "/nothing1/file.txt" [] @?=  ["/nothing1/file.txt"]
    expandPath' "/nothing2/file.txt" [undefined] @?=  ["/nothing2/file.txt"]
    expandPath' "/nothing3/file.txt" ["/home/luispedro/my-directory"] @?=  ["/nothing3/file.txt"]
    expandPath' "<>/nothing4/file.txt" ["/home/luispedro/my-directory1"] @?=  ["/home/luispedro/my-directory1/nothing4/file.txt"]
    expandPath' "<>/nothing4/file.txt" ["refs=/home/luispedro/my-directory1"] @?=  []
    expandPath' "<>/nothing/file.txt" ["/home/luispedro/my-directory"
                                      ,"/home/alternative/your-directory"] @?=  ["/home/luispedro/my-directory/nothing/file.txt"
                                                                                ,"/home/alternative/your-directory/nothing/file.txt"]
    expandPath' "<refs>/nothing/file.txt" ["/home/luispedro/my-directory"
                                      ,"/home/alternative/your-directory"] @?=  ["/home/luispedro/my-directory/nothing/file.txt"
                                                                                ,"/home/alternative/your-directory/nothing/file.txt"]
    expandPath' "<refs>/nothing/file.txt" ["refs=/home/luispedro/my-directory"
                                      ,"/home/alternative/your-directory"] @?=  ["/home/luispedro/my-directory/nothing/file.txt"
                                                                                ,"/home/alternative/your-directory/nothing/file.txt"]
    expandPath' "<refs>/nothing/file.txt" ["refs=/home/luispedro/my-directory"
                                      ,"nope=/home/alternative/your-directory"] @?=  ["/home/luispedro/my-directory/nothing/file.txt"]
    expandPath' "<refs>/nothing/file.txt" ["other=/home/luispedro/my-directory"
                                      ,"nope=/home/alternative/your-directory"] @?=  []
    expandPath' "<refs>/nothing/file.txt" [] @?= []
