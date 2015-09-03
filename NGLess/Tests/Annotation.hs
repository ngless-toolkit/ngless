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

ngo_gff_fp = "test_samples/sample.gtf.gz"
short_gff_fp = "test_samples/short.gtf"
very_short_sam_fp = "test_samples/very_short.sam"
very_short_gff_fp = "test_samples/very_short.gtf"

compareFiles fa fb = liftIO $ do
    ca <- BL.readFile fa
    cb <- BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equal."])
        (ca == cb)


case_annotate_gene_noStrand_union = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectUnion) False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOBool False)]


case_annotate_exon_noStrand_union = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_exon (_annotationRule IntersectUnion) False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOBool False)]


case_annotate_cds_noStrand_union = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_cds (_annotationRule IntersectUnion) False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOBool False)]


case_annotate_gene_noStrand_inters_strict = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectStrict) False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOBool False)]


case_annotate_gene_noStrand_inters_non_empty = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectNonEmpty) False False
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOBool False)]


case_annotate_gene_yesStrand_union = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectUnion) True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"

case_annotate_gene_yesStrand_nempty = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" ngo_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectNonEmpty) True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_nempty.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_nempty.txt"

case_annotate_gene_yesStrand_union_very_short = testNGLessIO $ do
    a <- _annotate very_short_sam_fp very_short_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectUnion) True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union_very_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"

case_annotate_gene_yesStrand_nempty_very_short = testNGLessIO $ do
    a <- _annotate very_short_sam_fp very_short_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectNonEmpty) True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_nempty_very_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"

case_annotate_gene_yesStrand_union_short = testNGLessIO $ do
    a <- _annotate "test_samples/sample.sam" short_gff_fp $ AnnotationOpts gff_features_gene (_annotationRule IntersectUnion) True True
    NGOAnnotatedSet p <- executeWrite (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOBool False)]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union_short.txt"



gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = [GFF.GffGene, GFF.GffCDS, GFF.GffExon]
gff_features_gene = [GFF.GffGene]
gff_features_cds = [GFF.GffCDS]
gff_features_exon = [GFF.GffExon]

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

case_filter_features_1 = filter (_matchFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (_matchFeatures [GFF.GffGene]) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (_matchFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (_matchFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (_matchFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []

