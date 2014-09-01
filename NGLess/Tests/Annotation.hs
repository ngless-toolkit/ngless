{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Annotation
    ( tgroup_Annotation
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Language
import FileManagement (readPossiblyCompressedFile)
import Interpretation.Write

import Annotation

tgroup_Annotation = $(testGroupGenerator)

ngo_gff_fp = Just "test_samples/sample.gtf"
short_gff_fp = Just "test_samples/short.gtf"
all_default_annot = do
  annotate "test_samples/sample.sam" ngo_gff_fp Nothing Nothing IntersectUnion True False >>= readPossiblyCompressedFile

-- test default values
case_annotate_features_default = do
    a1 <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion True False
    res1 <- readPossiblyCompressedFile a1
    res2 <- all_default_annot
    res1 @?= res2
  where feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile $ "test_samples/htseq-res/htseq_gene_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_exon_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile $ "test_samples/htseq-res/htseq_exon_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_exon_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "exon"]


case_annotate_cds_noStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectUnion False False
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile $ "test_samples/htseq-res/htseq_cds_noStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_cds_noStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "CDS"]


case_annotate_gene_noStrand_inters_strict = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectStrict False False
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile $ "test_samples/htseq-res/htseq_gene_noStrand_inters-strict.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-strict.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_noStrand_inters_non_empty = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty False False
    (NGOAnnotatedSet p) <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile "test_samples/htseq-res/htseq_gene_noStrand_inters-nempty.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_noStrand_inters-nempty.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_yesStrand_union = do
    a <- annotate "test_samples/sample.sam" ngo_gff_fp feats Nothing IntersectNonEmpty True True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile "test_samples/htseq-res/htseq_gene_yesStrand_union.txt"
    resHT @?= resNG
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]


case_annotate_gene_yesStrand_union_short = do
    a <- annotate "test_samples/sample.sam" short_gff_fp feats Nothing IntersectUnion True True
    NGOAnnotatedSet p <- writeToFile (NGOAnnotatedSet a) args
    resNG <- readPossiblyCompressedFile p
    resHT <- readPossiblyCompressedFile "test_samples/htseq-res/htseq_gene_yesStrand_union_short.txt"
    resNG @?= resHT
  where args = [("ofile", NGOString "test_samples/htseq-res/ngless_gene_yesStrand_union.txt"),("verbose", NGOSymbol "no")]
        feats = Just $ NGOList [NGOSymbol "gene"]

