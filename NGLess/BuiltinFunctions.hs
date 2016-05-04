{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
module BuiltinFunctions
    ( MethodName(..)
    , MethodInfo(..)
    , builtinFunctions
    , builtinMethods
    ) where

import Modules
import Language

data MethodInfo = MethodInfo
    { methodName :: MethodName
    , methodSelfType :: NGLType
    , methodArgType :: Maybe NGLType
    , methodReturnType :: NGLType
    , methodKwargsInfo :: [ArgInformation]
    , methodIsPure :: Bool
    } deriving (Eq, Show)

builtinFunctions =
    [Function (FuncName "fastq") (Just NGLString) [ArgCheckFileReadable] NGLReadSet fastqArgs False
    ,Function (FuncName "paired") (Just NGLString) [ArgCheckFileReadable] NGLReadSet pairedArgs False
    ,Function (FuncName "group") (Just (NGList NGLReadSet)) [] NGLReadSet groupArgs False
    ,Function (FuncName "samfile") (Just NGLString) [ArgCheckFileReadable] NGLMappedReadSet samfileArgs False
    ,Function (FuncName "unique") (Just NGLReadSet) [] NGLReadSet uniqueArgs False
    ,Function (FuncName "preprocess") (Just NGLReadSet) [] NGLVoid preprocessArgs False
    ,Function (FuncName "substrim") (Just NGLRead) [] NGLRead substrimArgs False
    ,Function (FuncName "map") (Just NGLReadSet) [] NGLMappedReadSet mapArgs False
    ,Function (FuncName "select") (Just NGLMappedReadSet) [] NGLMappedReadSet selectArgs False
    ,Function (FuncName "count") (Just NGLMappedReadSet) [] NGLCounts countArgs False
    ,Function (FuncName "write") (Just NGLAny) [] NGLVoid writeArgs False
    ,Function (FuncName "print") (Just NGLAny) [] NGLVoid [] False
    ]

groupArgs =
    [ArgInformation "name" True NGLString []
    ]

writeArgs =
    [ArgInformation "ofile" True NGLString []
    ,ArgInformation "format" False NGLSymbol [ArgCheckSymbol ["tsv", "csv", "bam", "sam"]]
    ,ArgInformation "verbose" False NGLBool []
    ]

countArgs =
    [ArgInformation "features" False (NGList NGLString) []
    ,ArgInformation "min" False NGLInteger []
    ,ArgInformation "multiple" False NGLSymbol [ArgCheckSymbol ["all1", "dist1", "1overN"]]
    ,ArgInformation "mode" False NGLSymbol [ArgCheckSymbol ["union", "intersection_strict", "intersection_non_empty"]]
    ,ArgInformation "gff_file" False NGLString []
    ,ArgInformation "functional_map" False NGLString []
    ,ArgInformation "keep_ambiguous" False NGLBool []
    ,ArgInformation "strand" False NGLBool []
    ,ArgInformation "norm" False NGLBool []
    ]

selectArgs =
    [ArgInformation "keep_if" False (NGList NGLSymbol) [ArgCheckSymbol ["mapped", "unmapped", "unique"]]
    ,ArgInformation "drop_if" False (NGList NGLSymbol) [ArgCheckSymbol ["mapped", "unmapped", "unique"]]
    ,ArgInformation "__oname" False NGLString []
    ]

fastqArgs =
    [ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
    ,ArgInformation "__perform_qc" False NGLBool []
    ]

samfileArgs =
    [ArgInformation "name" False NGLString []
    ]
pairedArgs =
    [ArgInformation "second" True NGLString []
    ,ArgInformation "singles" False NGLString []
    ,ArgInformation "__perform_qc" False NGLBool []
    ]

uniqueArgs =
    [ArgInformation "max_copies" False NGLInteger []]

preprocessArgs =
    [ArgInformation "keep_singles" False NGLBool []
    ,ArgInformation "__qc_input" False NGLBool []
    ]

mapArgs =
    [ArgInformation "reference" False NGLString []
    ,ArgInformation "fafile" False NGLString []
    ,ArgInformation "__extra_bwa" False (NGList NGLString) []
    ,ArgInformation "__oname" False NGLString []
    ]

substrimArgs =
    [ArgInformation "min_quality" True NGLInteger []
    ]


builtinMethods =
    [MethodInfo Mflag   NGLMappedRead (Just NGLSymbol) NGLBool [] True
    ,MethodInfo Mfilter NGLMappedRead Nothing NGLMappedRead filterArgs True
    ,MethodInfo Munique NGLMappedRead Nothing NGLMappedRead [] True
    ]
filterArgs =
    [ArgInformation "min_identity_pc" False NGLInteger []
    ,ArgInformation "min_match_size" False NGLInteger []
    ,ArgInformation "action" False NGLSymbol [ArgCheckSymbol ["drop", "unmap"]]
    ]

