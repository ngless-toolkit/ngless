{- Copyright 2013-2022 NGLess Authors
 - License: MIT
 -}
module BuiltinFunctions
    ( MethodName(..)
    , MethodInfo(..)
    , builtinModule
    , builtinMethods
    , findFunction
    ) where

import Data.List (find)
import Data.Default (def)
import qualified Data.Text as T

import NGLess.NGLEnvironment (NGLVersion(..))
import Modules
import Language

data MethodInfo = MethodInfo
    { methodName :: MethodName
    , methodSelfType :: NGLType
    , methodArgType :: Maybe NGLType
    , methodReturnType :: NGLType
    , methodKwargsInfo :: [ArgInformation] -- Unnamed argument is called "__0"
    , methodIsPure :: Bool
    , methodChecks :: [FunctionCheck]
    } deriving (Eq, Show)

findFunction :: [Module] -> FuncName -> Maybe Function
-- findFunction mods fn = trace (show mods) $ trace (show fn) $ find ((==fn) . funcName) $ concat (modFunctions <$> mods)
findFunction mods fn = find ((==fn) . funcName) $ concat (modFunctions <$> mods)

builtinModule :: NGLVersion -> Module
builtinModule ver@(NGLVersion majV minV) = def
    { modInfo = ModInfo "__builtin__" (T.pack $ show majV ++ "." ++ show minV)
    , modPath = ""
    , modFunctions = builtinFunctions ver
    }

printType = NGLUnion [NGLString, NGLInteger, NGLDouble]

builtinFunctions ver =
    [Function (FuncName "fastq") (Just NGLString) [ArgCheckFileReadable] NGLReadSet fastqArgs False []
    ,Function (FuncName "paired") (Just NGLString) [ArgCheckFileReadable] NGLReadSet pairedArgs False []
    ,Function (FuncName "group") (Just (NGList NGLReadSet)) [] NGLReadSet groupArgs False []
    ,Function (FuncName "samfile") (Just NGLString) [ArgCheckFileReadable] NGLMappedReadSet samfileArgs False []
    ,Function (FuncName "unique") (Just NGLReadSet) [] NGLReadSet uniqueArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "preprocess") (Just NGLReadSet) [] NGLReadSet preprocessArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "substrim") (Just NGLRead) [] NGLRead substrimArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "endstrim") (Just NGLRead) [] NGLRead endstrimArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "smoothtrim") (Just NGLRead) [] NGLRead smoothtrimArgs False [FunctionCheckReturnAssigned, FunctionCheckMinNGLessVersion (0, 11)]
    ,Function (FuncName "map") (Just NGLReadSet) [] NGLMappedReadSet mapArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "mapstats") (Just NGLMappedReadSet) [] NGLCounts mapStatsArgs False []
    ,Function (FuncName "select") (Just NGLMappedReadSet) [] NGLMappedReadSet selectArgs False []
    ,Function (FuncName "count") (Just NGLMappedReadSet) [] NGLCounts countArgs False [FunctionCheckReturnAssigned]
    ,Function (FuncName "__check_count") (Just NGLMappedReadSet) [] NGLCounts countCheckArgs False []
    ,Function (FuncName "countfile") (Just NGLString) [ArgCheckFileReadable] NGLCounts [] False [FunctionCheckReturnAssigned]
    ,Function (FuncName "write") (Just NGLAny) [] (if ver >= NGLVersion 1 4 then NGLString else NGLVoid) writeArgs False []

    ,Function (FuncName "print") (Just printType) [] NGLVoid [] False []
    ,Function (FuncName "println") (Just printType) [] NGLVoid [] False []

    ,Function (FuncName "read_int") (Just NGLString) [] NGLInteger [ArgInformation "on_empty_return" False NGLInteger []] False []
    ,Function (FuncName "read_double") (Just NGLString) [] NGLDouble [ArgInformation "on_empty_return" False NGLDouble []] False []

    ,Function (FuncName "__assert") (Just NGLBool) [] NGLVoid [] False []

    ,Function (FuncName "__merge_samfiles") (Just (NGList NGLString)) [] NGLMappedReadSet [] False []
    ]

groupArgs =
    [ArgInformation "name" True NGLString []
    ]

writeArgs =
    [ArgInformation "ofile" True NGLString [ArgCheckFileWritable]
    ,ArgInformation "format" False NGLSymbol [ArgCheckSymbol ["tsv", "csv", "bam", "sam"]]
    ,ArgInformation "format_flags" False NGLSymbol [ArgCheckMinVersion (0,7)
                                                   ,ArgCheckSymbol ["interleaved"]
                                                   ,ArgCheckSymbol ["always_3_fq_files"]
                                                   ]
    ,ArgInformation "verbose" False NGLBool []
    ,ArgInformation "comment" False NGLString []
    ,ArgInformation "auto_comments" False (NGList NGLSymbol) [ArgCheckSymbol ["date", "script", "hash"]]
    ,ArgInformation "compress_level" False NGLInteger [ArgCheckMinVersion (1,5)]
    ]

countArgs =
    [ArgInformation "features" False (NGList NGLString) []
    ,ArgInformation "subfeatures" False (NGList NGLString) []
    ,ArgInformation "min" False NGLInteger []
    ,ArgInformation "multiple" False NGLSymbol [ArgCheckSymbol ["all1", "dist1", "1overN", "unique_only"]]
    ,ArgInformation "mode" False NGLSymbol [ArgCheckSymbol ["union", "intersection_strict", "intersection_non_empty"]]
    ,ArgInformation "gff_file" False NGLString [ArgCheckFileReadable]
    ,ArgInformation "functional_map" False NGLString [ArgCheckFileReadable]
    ,ArgInformation "sense" False NGLSymbol [ArgCheckSymbol ["both", "sense", "antisense"], ArgCheckMinVersion (1,1)]
    ,ArgInformation "strand" False NGLBool [ArgDeprecated (1,1) "Use `sense` argument instead"]
    ,ArgInformation "norm" False NGLBool []
    ,ArgInformation "discard_zeros" False NGLBool []
    ,ArgInformation "include_minus1" False NGLBool []
    ,ArgInformation "normalization" False NGLSymbol [ArgCheckSymbol ["raw", "normed", "scaled", "fpkm"]]
    ,ArgInformation "reference" False NGLString [ArgCheckMinVersion (0,8)]
    ]

countCheckArgs = countArgs ++
    [ArgInformation "original_lno" False NGLInteger []
    ]


selectArgs =
    [ArgInformation "keep_if" False (NGList NGLSymbol) [ArgCheckSymbol ["mapped", "unmapped", "unique"]]
    ,ArgInformation "drop_if" False (NGList NGLSymbol) [ArgCheckSymbol ["mapped", "unmapped", "unique"]]
    ,ArgInformation "paired" False NGLBool []
    ,ArgInformation "__oname" False NGLString []
    ]

fastqArgs =
    [ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
    ,ArgInformation "interleaved" False NGLBool [ArgCheckMinVersion (1,1)]
    ,ArgInformation "__perform_qc" False NGLBool []
    ]

samfileArgs =
    [ArgInformation "name" False NGLString []
    ,ArgInformation "headers" False NGLString
                    [ArgCheckMinVersion (0,7)
                    ,ArgCheckFileReadable
                    ]
    ]
pairedArgs =
    [ArgInformation "second" True NGLString [ArgCheckFileReadable]
    ,ArgInformation "singles" False NGLString [ArgCheckFileReadable]
    ,ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
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
    ,ArgInformation "fafile" False NGLString [ArgCheckFileReadable]
    ,ArgInformation "mode_all" False NGLBool []
    ,ArgInformation "mapper" False NGLString []
    ,ArgInformation "block_size_megabases" False NGLInteger []
    ,ArgInformation "__extra_args" False (NGList NGLString) [ArgCheckMinVersion (1,1)]
    ,ArgInformation "__oname" False NGLString []
    ]

mapStatsArgs = []

substrimArgs =
    [ArgInformation "min_quality" True NGLInteger []
    ]

endstrimArgs =
    [ArgInformation "min_quality" True NGLInteger []
    ,ArgInformation "from_ends" False NGLSymbol [ArgCheckSymbol ["both", "3", "5"]]
    ]

smoothtrimArgs =
    [ArgInformation "min_quality" True NGLInteger []
    ,ArgInformation "window" True NGLInteger []
    ]

builtinMethods :: [MethodInfo]
builtinMethods =
    [
     -- NGLMappedReadSet
     MethodInfo (MethodName "flag")   NGLMappedRead (Just NGLSymbol) NGLBool flagArgs True []
    ,MethodInfo (MethodName "filter") NGLMappedRead Nothing NGLMappedRead filterArgs True []
    ,MethodInfo (MethodName "pe_filter") NGLMappedRead Nothing NGLMappedRead [] True []
    ,MethodInfo (MethodName "some_match") NGLMappedRead (Just NGLString) NGLBool [] True []
    ,MethodInfo (MethodName "unique") NGLMappedRead Nothing NGLMappedRead [] True []
    ,MethodInfo (MethodName "allbest") NGLMappedRead Nothing NGLMappedRead [] True [FunctionCheckMinNGLessVersion (0,9)]

    -- NGLRead
    ,MethodInfo (MethodName "avg_quality") NGLRead Nothing NGLDouble [] True []
    ,MethodInfo (MethodName "fraction_at_least") NGLRead (Just NGLInteger) NGLDouble [] True []
    ,MethodInfo (MethodName "n_to_zero_quality") NGLRead Nothing NGLRead [] True [FunctionCheckMinNGLessVersion (0,8)]

    -- NGLReadSet
    ,MethodInfo (MethodName "name") NGLReadSet Nothing NGLString [] True []

    -- NGLDouble
    ,MethodInfo (MethodName "to_string") NGLDouble Nothing NGLString [] True []

    -- NGLInteger
    ,MethodInfo (MethodName "to_string") NGLInteger Nothing NGLString [] True []
    ]

filterArgs =
    [ArgInformation "min_identity_pc" False NGLInteger []
    ,ArgInformation "min_match_size" False NGLInteger []
    ,ArgInformation "max_trim" False NGLInteger [ArgCheckMinVersion (0,7)]
    ,ArgInformation "action" False NGLSymbol [ArgCheckSymbol ["drop", "unmatch"]]
    ,ArgInformation "reverse" False NGLBool []
    ]

flagArgs =
    [ArgInformation "__0" False NGLSymbol [ArgCheckSymbol ["mapped", "unmapped"]]
    ]
