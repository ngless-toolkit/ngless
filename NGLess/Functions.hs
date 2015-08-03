{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Functions
    ( builtinFunctions
    ) where

import Modules
import Language

builtinFunctions =
    [Function Ffastq (Just NGLString) NGLReadSet fastqArgs False
    ,Function Fsamfile (Just NGLString) NGLMappedReadSet samfileArgs False
    ,Function Fpaired (Just NGLString) NGLReadSet pairedArgs False
    ,Function Funique (Just NGLReadSet) NGLReadSet uniqueArgs False
    ,Function Fpreprocess (Just NGLReadSet) NGLVoid preprocessArgs False
    ,Function Fsubstrim (Just NGLRead) NGLRead substrimArgs False
    ,Function Fmap (Just NGLReadSet) NGLMappedReadSet mapArgs False
    ,Function Fselect (Just NGLMappedReadSet) NGLMappedReadSet selectArgs False
    ,Function Fcount (Just NGLMappedReadSet) NGLCounts countArgs False
    ,Function Fannotate (Just NGLMappedReadSet) NGLMappedReadSet annotateArgs False
    ,Function Fwrite (Just NGLAny) NGLVoid writeArgs False
    ,Function Fprint (Just NGLAny) NGLVoid [] False
    ]

annotateArgs =
    [ArgInformation "features" False (NGList NGLSymbol) (Just ["gene", "cds", "exon"])
    ,ArgInformation "mode" False NGLSymbol (Just ["union", "intersection_strict", "intersection_non_empty"])
    ,ArgInformation "gff" False NGLString Nothing
    ,ArgInformation "keep_ambiguous" False NGLBool Nothing
    ,ArgInformation "strand" False NGLBool Nothing
    ]

writeArgs =
    [ArgInformation "ofile" True NGLString Nothing
    ,ArgInformation "format" False NGLSymbol (Just ["tsv", "csv", "bam", "sam"])
    ,ArgInformation "verbose" False NGLBool Nothing
    ]

countArgs =
    [ArgInformation "counts" False (NGList NGLSymbol) (Just ["gene", "cds", "exon"])
    ,ArgInformation "min" False NGLInteger Nothing
    ]

selectArgs =
    [ArgInformation "keep_if" False (NGList NGLSymbol) (Just ["mapped", "unmapped"])
    ,ArgInformation "drop_if" False (NGList NGLSymbol) (Just ["mapped", "unmapped"])
    ]

fastqArgs =
    [ArgInformation "encoding" False NGLSymbol (Just ["auto", "33", "64", "sanger", "solexa"])]

samfileArgs = []
pairedArgs =
    [ArgInformation "second" True NGLString Nothing
    ,ArgInformation "singles" False NGLString Nothing
    ]

uniqueArgs =
    [ArgInformation "max_copies" False NGLInteger Nothing]

preprocessArgs =
    []

mapArgs =
    [ArgInformation "reference" True NGLString Nothing
    ]

substrimArgs =
    [ArgInformation "min_quality" True NGLInteger Nothing
    ]


