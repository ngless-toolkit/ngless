{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Annotation
    ( tgroup_Annotation
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as BL

import Language
import Interpretation.Write
import qualified Data.GFF as GFF

import Annotation

tgroup_Annotation = $(testGroupGenerator)

ngo_gff_fp = Just "test_samples/sample.gtf"
short_gff_fp = Just "test_samples/short.gtf"

compareFiles fa fb = do
    ca <- BL.readFile fa
    cb <- BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equal."])
        (ca == cb)

-- test default values
case_annotate_features_default = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing IntersectUnion True False
    b <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion True False
    compareFiles a b
  where feats = Just ["gene"]

e = "testing_tmp_dir"

case_annotate_gene_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]


case_annotate_exon_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just ["exon"]


case_annotate_cds_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just ["CDS"]


case_annotate_gene_noStrand_inters_strict = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectStrict False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]


case_annotate_gene_noStrand_inters_non_empty = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]


case_annotate_gene_yesStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"


case_annotate_gene_yesStrand_nstrict = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectStrict False True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_nstrict.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_nstrict.txt"

case_annotate_gene_yesStrand_nempty = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty False True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_nempty.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_nempty.txt"


case_annotate_gene_yesStrand_union_short = do
    a <- annotate "test_samples/sample.sam" short_gff_fp feats Nothing IntersectUnion False True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args e
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union_short.txt"



gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = Just ["gene", "cds", "exon"]
gff_features_gene = Just ["gene"]
gff_features_cds = Just ["cds"]

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

case_filter_features_1 = filter (_filterFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (_filterFeatures Nothing) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (_filterFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (_filterFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (_filterFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []

