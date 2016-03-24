{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Count
    ( tgroup_Count
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.IntervalMap.Strict as IM
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

import qualified Data.GFF as GFF
import Interpretation.Count
import Tests.Utils
import Data.GFF
import NGLess


tgroup_Count = $(testGroupGenerator)

ngo_gff_fp = "test_samples/sample.gtf"
short_gff_fp = "test_samples/short.gtf"

very_short_sam = "test_samples/very_short.sam"
very_short_gff = "test_samples/very_short.gtf"


equivalentLine (a,b) = parse a == parse b
    where
        parse :: BL.ByteString -> (BL.ByteString, Double)
        parse ell = let [n,v] = BL8.split '\t' ell in (n, read $ BL8.unpack v)

compareFiles fa fb = liftIO $ do
    ca <- BL8.lines <$> BL.readFile fa
    cb <- BL8.lines <$> BL.readFile fb
    assertBool (concat ["Expected files ", fa, " and ", fb, " to be equivalent"])
        (all equivalentLine (zip ca cb))

annotate_count_compare htseq_version sam gff opts = testNGLessIO $ do
    ann <- loadAnnotator (AnnotateGFF gff) opts
    p <- performCount sam "testing" ann opts
    compareFiles p ("test_samples/htseq-res/" ++htseq_version)

defCountOpts =
    CountOpts
    { optFeatures = []
    , optIntersectMode = annotationRule IntersectUnion
    , optStrandSpecific = False
    , optKeepAmbiguous = False
    , optMinCount = 0.0
    , optMMMethod = MMCountAll
    , optDelim = "\t"
    , optNormSize = False
    }

case_annotate_gene_noStrand_union =
    annotate_count_compare
        "htseq_gene_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffGene] }

case_annotate_exon_noStrand_union =
    annotate_count_compare
        "htseq_exon_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffExon] }

case_annotate_cds_noStrand_union =
    annotate_count_compare
        "htseq_cds_noStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffCDS], optMinCount = 1.0 }

case_annotate_gene_noStrand_inters_strict =
    annotate_count_compare
        "htseq_gene_noStrand_inters-strict.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectStrict }

case_annotate_gene_noStrand_inters_non_empty =
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectNonEmpty }


case_annotate_gene_noStrand_inters_non_empty_diff = -- this is a regression test
    annotate_count_compare
        "htseq_gene_noStrand_inters-nempty_diff.txt"
        "test_samples/nonempty_diff.sam" "test_samples/nonempty_diff.gtf"
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectNonEmpty }


case_annotate_gene_yesStrand_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffGene], optStrandSpecific = True, optKeepAmbiguous = True }

case_annotate_gene_yesStrand_nempty =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty.txt"
        "test_samples/sample.sam" ngo_gff_fp
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectNonEmpty, optStrandSpecific = True }

case_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_short.txt"
        "test_samples/sample.sam" short_gff_fp
        $ defCountOpts { optFeatures = [GffGene], optStrandSpecific = True, optKeepAmbiguous = True, optMinCount = 1.0 }

case_very_short_annotate_union =
    annotate_count_compare
        "htseq_gene_yesStrand_union_very_short.txt"
        very_short_sam very_short_gff
        $ defCountOpts { optFeatures = [GffGene], optStrandSpecific = True, optKeepAmbiguous = True }

case_very_short_annotate_nempty_yesStrand =
    annotate_count_compare
        "htseq_gene_yesStrand_nempty_very_short.txt"
        very_short_sam very_short_gff
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectNonEmpty, optStrandSpecific = True }

case_very_short_annotate_nempty_noStrand =
    annotate_count_compare
        "htseq_gene_noStrand_nempty_very_short.txt"
        very_short_sam very_short_gff
        $ defCountOpts { optFeatures = [GffGene], optIntersectMode = annotationRule IntersectNonEmpty }


gff_structure_Exon = GFF.GffLine "chrI" "unknown" GFF.GffExon 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_CDS = GFF.GffLine "chrI" "unknown" GFF.GffCDS 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"
gff_structure_Gene = GFF.GffLine "chrI" "unknown" GFF.GffGene 4124 4358 Nothing GFF.GffNegStrand (-1) "gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"


gff_features_all = [GFF.GffGene, GFF.GffCDS, GFF.GffExon]
gff_features_gene = [GFF.GffGene]
gff_features_cds = [GFF.GffCDS]

gff_lines_ex = [gff_structure_Exon,gff_structure_CDS,gff_structure_Gene]

case_filter_features_1 = filter (matchFeatures gff_features_all) gff_lines_ex @?= gff_lines_ex
case_filter_features_2 = filter (matchFeatures [GFF.GffGene]) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_3 = filter (matchFeatures gff_features_gene) gff_lines_ex @?= [gff_structure_Gene]
case_filter_features_4 = filter (matchFeatures gff_features_cds) gff_lines_ex @?= [gff_structure_CDS]
case_filter_features_5 = filter (matchFeatures gff_features_cds) [gff_structure_Exon,gff_structure_Exon,gff_structure_Gene] @?= []

listNub = S.toList . S.fromList
case_load_very_short = do
    GFFAnnotator immap headers szmap <- testNGLessIO
                $ loadAnnotator (AnnotateGFF very_short_gff) defCountOpts  { optFeatures = [GffGene] }
    let usedIDs = map snd $ concat $ concatMap IM.elems $ M.elems immap
    length (listNub usedIDs ) @?= length headers
    minimum usedIDs @?= 0
    maximum usedIDs @?= length headers - 1
    M.size szmap @?= length headers
    M.lookup "WBGene00010199" szmap @?= Just (721-119+1)


short3 :: B.ByteString
short3 =
    "V\tprotein_coding\tgene\t7322\t8892\t.\t-\t.\tgene_id \"WBGene00008825\"; gene_name \"F14H3.6\"; gene_source \"ensembl\"; gene_biotype \"protein_coding\";\n\
    \X\tprotein_coding\tgene\t140\t218\t.\t+\t.\tgene_id \"WBGene00020330\"; gene_name \"T07H6.1\"; gene_source \"ensembl\"; gene_biotype \"protein_coding\";\n\
    \X\tprotein_coding\tgene\t632\t733\t.\t+\t.\tgene_id \"WBGene00000526\"; gene_name \"clc-5\"; gene_source \"ensembl\"; gene_biotype \"protein_coding\";\n"

-- this is a regression test
case_load_gff_order = do
    fp <- testNGLessIO $ asTempFile short3 "gtf"
    GFFAnnotator immap headers szmap <- testNGLessIO
                $ loadAnnotator (AnnotateGFF fp) defCountOpts  { optFeatures = [GffGene] }
    let [h] = map snd . concat . IM.elems  . fromJust $ M.lookup "V" immap
    (headers !! h) @?= "WBGene00008825"

