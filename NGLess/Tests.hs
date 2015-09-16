{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
-- Unit tests are their own programme.

module Main where

import Test.Framework
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Control.Monad.Except
import Control.Applicative
import Text.Parsec (parse)
import Text.Parsec.Combinator (eof)

import System.Directory (removeFile
                        ,removeDirectoryRecursive
                        ,createDirectoryIfMissing
                        ,doesFileExist
                        )
import System.Console.CmdArgs.Verbosity

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


import Data.Convertible

import qualified Data.Vector.Unboxed as V

import Language
import Interpret
import Tokens
import Types
import Substrim
import FileManagement
import Data.FastQStatistics
import Interpretation.FastQ
import Configuration
import NGLess

import Interpretation.Map
import Unique

import Data.FastQ
import Data.Sam
import Utils.Utils
import Utils.Vector
import qualified Data.GFF as GFF

import Tests.Utils
import Tests.FastQ
import Tests.Validation
import Tests.Types (tgroup_Types)
import Tests.Annotation (tgroup_Annotation)
import Tests.Parse (tgroup_Parse)

test_FastQ = [tgroup_FastQ]
test_Validation = [tgroup_Validation]
test_Annotation = [tgroup_Annotation]
test_Parse      = [tgroup_Parse]
test_Types      = [tgroup_Types]

-- The main test driver sets verbosity to Quiet to avoid extraneous output and
-- then uses the automatically generated function
main = do
    setVerbosity Quiet
    setupTestConfiguration
    $(defaultMainGenerator)
    removeDirectoryRecursive "testing_directory_tmp"

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



--- SETUP to reduce imports.
-- test array: "\n\v\f{zo\n\v\NUL" -> [10,11,12,123,122,111,10,11,0]
-- test cutoff: chr 20 -> '\DC4'

--Property 1: For every s, the size must be always smaller than the input
prop_substrim_maxsize s = st >= 0 && e <= B.length (B.pack s)
    where (st,e) = subtrimPos (B.pack s) '\DC4'

-- Property 2: substrim should be idempotent
prop_substrim_idempotent s = st == 0 && e == B.length s1
    where
        s1 = cutByteString (B.pack s) (subtrimPos (B.pack s) '\DC4')
        (st,e) = subtrimPos s1 '\DC4'

case_substrim_normal_exec =  subtrimPos "\n\v\f{zo\n\v\NUL" '\DC4' @?= (3,3)
case_substrim_empty_quals = subtrimPos "" '\DC4' @?= (0,0)

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
sr i s q = NGOShortRead (ShortRead i s q)

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

-- Sam operations

samLineFlat = "IRIS:7:3:1046:1723#0\t4\t*\t0\t0\t37M\t*\t0\t0\tAAAAAAAAAAAAAAAAAAAAAAA\taaaaaaaaaaaaaaaaaa`aa`^\tAS:i:0  XS:i:0"
samLine = SamLine
            { samQName = "IRIS:7:3:1046:1723#0"
            , samFlag = 4
            , samRName = "*"
            , samPos = 0
            , samMapq = 0
            , samCigar = "37M"
            , samRNext = "*"
            , samPNext = 0
            , samTLen = 0
            , samSeq = "AAAAAAAAAAAAAAAAAAAAAAA"
            , samQual = "aaaaaaaaaaaaaaaaaa`aa`^"
            }

case_isAligned_sam = isAligned (samLine {samFlag = 16}) @? "Should be aligned"
case_isAligned_raw = isAligned (head . readAlignments $ r) @? "Should be aligned"
    where
        r = "SRR070372.3\t16\tV\t7198336\t21\t26M3D9M3D6M6D8M2D21M\t*\t0\t0\tCCCTTATGCAGGTCTTAACACAATTCTTGTATGTTCCATCGTTCTCCAGAATGAATATCAATGATACCAA\t014<<BBBBDDFFFDDDDFHHFFD?@??DBBBB5555::?=BBBBDDF@BBFHHHHHHHFFFFFD@@@@@\tNM:i:14\tMD:Z:26^TTT9^TTC6^TTTTTT8^AA21\tAS:i:3\tXS:i:0"

case_isNotAligned = (not $ isAligned (samLine {samFlag = 4})) @? "Should not be aligned"

case_isUnique = isUnique (samLine {samMapq = 5}) @? "Should be unique"
case_isNotUnique = (not $ isUnique (samLine {samMapq = 0})) @? "Should not be unique"

case_read_one_Sam_Line = readAlignments samLineFlat @?= [samLine]
case_read_mul_Sam_Line = readAlignments (L.unlines $ replicate 10 samLineFlat) @?= replicate 10 samLine


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
        (length $ B.lines res') @?= (16 :: Int)
        removeFile "test_samples/sample20_post.fq"

case_map_script = case parsetest map_s >>= checktypes [] of
    Left err -> assertFailure (show err)
    Right expr -> do
        testNGLessIO $ (interpret []) . nglBody $ expr
        res' <- readPossiblyCompressedFile "test_samples/sample20_mapped.sam"
        _calcSamStats res' @?= (5,0,0,0)
        removeFile "test_samples/sample20_mapped.sam"

map_s = "ngless '0.0'\n\
    \input = fastq('test_samples/sample20.fq')\n\
    \preprocess(input) using |read|:\n\
    \    if len(read) < 20:\n\
    \        discard\n\
    \mapped = map(input,reference='sacCer3')\n\
    \write(mapped, ofile='test_samples/sample20_mapped.sam',format={sam})\n"


-- Test compute stats

case_compute_stats_lc = do
    contents <- readPossiblyCompressedFile "test_samples/sample_small.fq"
    (convert . lc $ statsFromFastQ contents) @?= ']'

-- Parse GFF lines

case_read_annotation_comp = do
    c <- readPossiblyCompressedFile "test_samples/sample.gtf.gz"
    length (GFF.readAnnotations c) @?= 98994

case_read_annotation_uncomp = do
    c <- readPossiblyCompressedFile "test_samples/sample.gtf.gz"
    length (GFF.readAnnotations c) @?= 98994


gff_line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_attributes = "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";" 
gff_structure  = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "Y74C9A.3"


case_check_attr_tag_1 = GFF.checkAttrTag "id = 10;" @?= '='
case_check_attr_tag_2 = GFF.checkAttrTag "id 10;" @?= ' '

case_trim_attrs_1  = GFF.trimString " x = 10" @?= "x = 10"
case_trim_attrs_2  = GFF.trimString " x = 10 " @?= "x = 10"
case_trim_attrs_3  = GFF.trimString "x = 10 " @?= "x = 10"
case_trim_attrs_4  = GFF.trimString "x = 10" @?= "x = 10"


case_parse_gff_line = GFF.readLine gff_line @?= gff_structure
case_parse_gff_atributes = GFF.parseGffAttributes gff_attributes @?= [("gene_id","Y74C9A.3"), ("transcript_id" ,"NM_058260"), ("gene_name", "Y74C9A.3"), ("p_id", "P23728"), ("tss_id", "TSS14501")]

-- teste parseGffAttributes
case_parse_gff_atributes_normal_1 = GFF.parseGffAttributes "ID=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("ID","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_normal_2 = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI;" @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]
case_parse_gff_atributes_trail_del_space = GFF.parseGffAttributes "gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI; " @?= [("gene_id","chrI"),("dbxref","NCBI:NC_001133"),("Name","chrI")]


----- VectorOperations.hs
case_zero_vec = do
  v <- zeroVec 4 >>= V.freeze
  v @?= V.fromList [0,0,0,0]

case_calc_sam_stats = do
  r <- _calcSamStats <$> readPossiblyCompressedFile "test_samples/sample.sam.gz"
  r @?=  (3072,1610,1554,0)

--- Unique.hs

--File "test_samples/data_set_repeated.fq" has 216 reads in which 54 are unique.

case_num_files_1 = do
  n <- _numFiles "test_samples/data_set_repeated.fq"
  n @?= 1

case_num_files_2 = do -- github rejects files with more than 100MB
  n <- _numFiles "test_samples/sample.sam"
  n @?= 1


make_unique_test n = let enc = SolexaEncoding in do
    c <- readReadSet enc "test_samples/data_set_repeated.fq"
    ds <- testNGLessIO $ do
        p <- _writeToNFiles "test_samples/data_set_repeated.fq" enc c
        liftIO $ _readNFiles enc n p
    length ds @?=  (n * 54)

case_unique_1_read = make_unique_test 1
case_unique_2_read = make_unique_test 2
case_unique_3_read = make_unique_test 3

case_unique_5 = let enc = SolexaEncoding in do
    c <- readReadSet enc "test_samples/data_set_repeated.fq"
    ds <- testNGLessIO $ do
        p <- _writeToNFiles "test_samples/data_set_repeated.fq" enc c
        liftIO $ _readNFiles enc 5 p
    length ds @?=  (4 * 54) -- there are only 4 copies!

-- PerBaseQualityScores 

case_calc_perc_med = _calcPercentile bps eT 0.5 @?= 4
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 21 -> mul: 0,5  +- 11 in arr = 13 index 4

case_calc_perc_lq = _calcPercentile bps eT 0.25 @?= 2
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 21 -> mul: 0,25 -> 6 in arr = 6 index 2

case_calc_perc_uq = _calcPercentile bps eT 0.75 @?= 5
    where bps = V.fromList [3,1,2,3,4,5,1,2] -- [3,4,6,9,13,18,19,21] -> arr
          eT  = V.sum bps -- 8 -> mul: 0,75 -> 16 in arr = 18 index 5


-- negative tests quality on value 60 char ';'. Value will be 60 - 64 which is -4
case_calc_statistics_negative = do
    s <- statsFromFastQ <$> readPossiblyCompressedFile "test_samples/sample_low_qual.fq"
    head (stats' s) @?= (-4,-4,-4,-4)
  where stats' s = calculateStatistics s (guessEncoding . lc $ s)

-- low positive tests quality on 65 char 'A'. Value will be 65-64 which is 1.
case_calc_statistics_low_positive = do
    s <- statsFromFastQ <$> readPossiblyCompressedFile "test_samples/sample_low_qual.fq"
    last (stats' s) @?= (1,1,1,1)
  where stats' s = calculateStatistics s (guessEncoding . lc $ s)


case_calc_statistics_normal = do
    s <- statsFromFastQ <$> readPossiblyCompressedFile "test_samples/data_set_repeated.fq"
    head (stats' s) @?= (25,33,31,33)
  where stats' s = calculateStatistics s (guessEncoding . lc $ s)

case_test_setup_html_view = do
    setupHtmlViewer "testing_tmp_dir_html"
    ex <- doesFileExist "testing_tmp_dir_html/index.html"
    assertBool "index.html should be present after setupHtmlViewer" ex
    removeDirectoryRecursive "testing_tmp_dir_html/"

-- MapOperations

-- ProcessFastQ
low_char_int = (lc . statsFromFastQ) <$> readPossiblyCompressedFile "test_samples/sample.fq.gz"

case_read_and_write_fastQ = do
    enc <- guessEncoding <$> low_char_int
    rs <- readReadSet enc "test_samples/sample.fq.gz"
    testNGLessIO $ do
        fp <- writeTempFastQ "test_samples/sample.fq.gz" rs enc
        newrs <- liftIO $ readReadSet enc fp
        liftIO $ newrs @?= rs

-- "test_samples/sample.fq.gz" has 33 as lowest char from the initial data set
case_read_fastQ_store_enc = do
    nt <- testNGLessIO $ generateDirId fp
    createDirectoryIfMissing False $ dstDirBef nt
    createDirectoryIfMissing False $ dstDirAft nt
    (NGOReadSet1 eb _) <- testNGLessIO $ executeQProc Nothing   fp
    (NGOReadSet1 ea _) <- testNGLessIO $ executeQProc (Just eb) fp
    removeDirectoryRecursive $ dstDirBef nt -- delete test generated data.
    removeDirectoryRecursive $ dstDirAft nt -- delete test generated data.
    eb @?= ea
  where fp = "test_samples/sample.fq.gz"
        dstDirBef = (++ "$beforeQC")
        dstDirAft = (++ "$afterQC")

