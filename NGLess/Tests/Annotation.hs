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
  where feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_exon_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "exon"]


case_annotate_cds_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "CDS"]


case_annotate_gene_noStrand_inters_strict = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectStrict False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_noStrand_inters_non_empty = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty False False
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_yesStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty True True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        ofile = "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"

case_annotate_gene_yesStrand_union_short = do
    a <- annotate "test_samples/sample.sam" short_gff_fp feats Nothing IntersectUnion True True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    compareFiles p "test_samples/htseq-res/htseq_gene_yesStrand_union_short.txt"
  where args = [("ofile", NGOString ofile),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]
        ofile = "test_samples/htseq-res/ngless_gene_yesStrand_union_short.txt"
