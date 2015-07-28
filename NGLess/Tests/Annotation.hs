{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Annotation
    ( tgroup_Annotation
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL

import Language
import NGLess
import Interpretation.Annotation
import Interpretation.Write
import qualified Data.GFF as GFF


tgroup_Annotation = $(testGroupGenerator)

ngo_gff_fp = Just "test_samples/sample.gtf.gz"
short_gff_fp = Just "test_samples/short.gtf"

compareFiles fa fb = liftIO $ do
    ca <- BL.readFile fa
    cb <- BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equal."])
        (ca == cb)

-- test default values
case_annotate_features_default = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp Nothing          Nothing IntersectUnion True False
    b <- annotate "test_samples/sample.sam" ngo_gff_fp (Just ["gene"])  Nothing IntersectUnion True False
    compareFiles a b

case_annotate_gene_noStrand_union = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOBool False)]
        feats = Just ["gene"]


case_annotate_exon_noStrand_union = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOBool False)]
        feats = Just ["exon"]


case_annotate_cds_noStrand_union = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOBool False)]
        feats = Just ["CDS"]


case_annotate_gene_noStrand_inters_strict = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectStrict False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOBool False)]
        feats = Just ["gene"]


case_annotate_gene_noStrand_inters_non_empty = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOBool False)]
        feats = Just ["gene"]


case_annotate_gene_yesStrand_union = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"

case_annotate_gene_yesStrand_union_short = testNGLessIO $ do
    a <- annotate "test_samples/sample.sam" short_gff_fp feats Nothing IntersectUnion True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        feats = Just ["gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union_short.txt"



gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = [GFF.GffGene, GFF.GffCDS, GFF.GffExon]
gff_features_gene = [GFF.GffGene]
gff_features_cds = [GFF.GffCDS]

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

case_filter_features_1 = filter (_matchFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (_matchFeatures [GFF.GffGene]) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (_matchFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (_matchFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (_matchFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []

