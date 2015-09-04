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
import Interpretation.Count
import Interpretation.Annotation
import Interpretation.Write
import Data.GFF
import qualified Data.GFF as GFF


tgroup_Annotation = $(testGroupGenerator)

ngo_gff_fp = "test_samples/sample.gtf"
short_gff_fp = "test_samples/short.gtf"
very_short_sam_fp = "test_samples/very_short.sam"
very_short_gff_fp = "test_samples/very_short.gtf"

very_short_sam = "test_samples/very_short.sam"
very_short_gff = "test_samples/very_short.gtf"

compareFiles fa fb = liftIO $ do
    ca <- BL.readFile fa
    cb <- BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equal."])
        (ca == cb)

annotate_count_compare htseq_version sam gff minv opts = testNGLessIO $ do
    (a,h) <- _annotate sam gff opts
    NGOCounts p <- executeCount (NGOAnnotatedSet a h) args
    compareFiles p ("test_samples/htseq-res/" ++htseq_version)
  where args = [("verbose", NGOBool False), ("min", NGOInteger minv)]


case_annotate_gene_noStrand_union =
    annotate_count_compare
        "htseq_gene_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectUnion) False False


case_annotate_exon_noStrand_union =
    annotate_count_compare
        "htseq_exon_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffExon] (_annotationRule IntersectUnion) False False

case_annotate_cds_noStrand_union =
    annotate_count_compare
        "htseq_cds_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp 0
        $ AnnotationOpts [GffCDS] (_annotationRule IntersectUnion) False False

case_annotate_gene_noStrand_inters_strict =
    annotate_count_compare
        "htseq_gene_noStrand_inters-strict.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectStrict) False False

case_annotate_gene_noStrand_inters_non_empty =
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectNonEmpty) False False


case_annotate_gene_noStrand_inters_non_empty_diff = -- this is a regression test
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty_diff.txt"
        "test_samples/nonempty_diff.sam" "test_samples/nonempty_diff.gtf" (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectNonEmpty) False False


case_annotate_gene_yesStrand_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectUnion) True True

case_annotate_gene_yesStrand_nempty =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectNonEmpty) True False

case_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_short.txt"
        "test_samples/sample.sam" short_gff_fp 0
        $ AnnotationOpts [GffGene] (_annotationRule IntersectUnion) True True

case_very_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_very_short.txt"
        very_short_sam very_short_gff (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectUnion) True True

case_very_short_annotate_nempty_yesStrand =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty_very_short.txt"
        very_short_sam very_short_gff (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectNonEmpty) True False

case_very_short_annotate_nempty_noStrand =
    annotate_count_compare
        "htseq_gene_noStrand_nempty_very_short.txt"
        very_short_sam very_short_gff (-1)
        $ AnnotationOpts [GffGene] (_annotationRule IntersectNonEmpty) False False


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

