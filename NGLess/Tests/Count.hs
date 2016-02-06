{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Count
    ( tgroup_Count
    ) where

import Control.Applicative
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import Language
import NGLess
import Interpretation.Count
import Data.GFF
import qualified Data.GFF as GFF


tgroup_Count = $(testGroupGenerator)

ngo_gff_fp = "test_samples/sample.gtf"
short_gff_fp = "test_samples/short.gtf"
very_short_sam_fp = "test_samples/very_short.sam"
very_short_gff_fp = "test_samples/very_short.gtf"

very_short_sam = "test_samples/very_short.sam"
very_short_gff = "test_samples/very_short.gtf"


equivalentLine (a,b) = parse a == parse b
    where
        parse :: BL.ByteString -> (BL.ByteString, Double)
        parse ell = let [n,v] = BL8.split '\t' ell in (n, read $ BL8.unpack v)

compareFiles fa fb = liftIO $ do
    ca <- BL8.lines <$> BL.readFile fa
    cb <- BL8.lines <$> BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equal."])
        (all equivalentLine (zip ca cb))

annotate_count_compare htseq_version sam gff opts = testNGLessIO $ do
    ann <- loadAnnotator (AnnotateGFF gff) opts
    p <- performCount sam "testing" ann opts
    compareFiles p ("test_samples/htseq-res/" ++htseq_version)


case_annotate_gene_noStrand_union =
    annotate_count_compare
        "htseq_gene_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectUnion) False False 0.0 MMCountAll "\t"

case_annotate_exon_noStrand_union =
    annotate_count_compare
        "htseq_exon_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffExon] (annotationRule IntersectUnion) False False 0.0 MMCountAll "\t"

case_annotate_cds_noStrand_union =
    annotate_count_compare
        "htseq_cds_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffCDS] (annotationRule IntersectUnion) False False 1.0 MMCountAll "\t"

case_annotate_gene_noStrand_inters_strict =
    annotate_count_compare
        "htseq_gene_noStrand_inters-strict.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectStrict) False False 0.0 MMCountAll "\t"

case_annotate_gene_noStrand_inters_non_empty =
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectNonEmpty) False False 0.0 MMCountAll "\t"


case_annotate_gene_noStrand_inters_non_empty_diff = -- this is a regression test
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty_diff.txt"
        "test_samples/nonempty_diff.sam" "test_samples/nonempty_diff.gtf"
        $ CountOpts [GffGene] (annotationRule IntersectNonEmpty) False False 0.0 MMCountAll "\t"


case_annotate_gene_yesStrand_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectUnion) True True 0.0 MMCountAll "\t"

case_annotate_gene_yesStrand_nempty =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectNonEmpty) True False 0.0 MMCountAll "\t"

case_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_short.txt"
        "test_samples/sample.sam" short_gff_fp
        $ CountOpts [GffGene] (annotationRule IntersectUnion) True True 1.0 MMCountAll "\t"

case_very_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_very_short.txt"
        very_short_sam very_short_gff
        $ CountOpts [GffGene] (annotationRule IntersectUnion) True True 0.0 MMCountAll "\t"

case_very_short_annotate_nempty_yesStrand =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty_very_short.txt"
        very_short_sam very_short_gff
        $ CountOpts [GffGene] (annotationRule IntersectNonEmpty) True False 0.0 MMCountAll "\t"

case_very_short_annotate_nempty_noStrand =
    annotate_count_compare
        "htseq_gene_noStrand_nempty_very_short.txt"
        very_short_sam very_short_gff
        $ CountOpts [GffGene] (annotationRule IntersectNonEmpty) False False 0.0 MMCountAll "\t"


gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = [GFF.GffGene, GFF.GffCDS, GFF.GffExon]
gff_features_gene = [GFF.GffGene]
gff_features_cds = [GFF.GffCDS]
gff_features_exon = [GFF.GffExon]

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

case_filter_features_1 = filter (matchFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (matchFeatures [GFF.GffGene]) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (matchFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (matchFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (matchFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []

