{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Types
    ( tgroup_Types
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Text as T

import Tests.Utils
import Types
import Language

tgroup_Types = $(testGroupGenerator)

isOkTypes :: Script -> IO ()
isOkTypes script = case checktypes [] script of
    (Right _) -> return ()
    (Left err) -> assertFailure ("Type error on good code (error was '"++T.unpack err++"'")

isOkTypesText scriptText = case parsetest scriptText >>= checktypes [] of
    (Right _) -> return ()
    (Left err) -> assertFailure ("Type error on good code (error was '"++T.unpack err++"') for script: '"++T.unpack scriptText++"'")

isErrorText scriptText = isError $ parsetest scriptText>>= checktypes []


case_bad_type_fastq = isError $ checktypes [] (Script Nothing [(0,FunctionCall (FuncName "fastq") (ConstNum 3) [] Nothing)])
case_good_type_fastq = isOkTypes (Script Nothing [(0,FunctionCall (FuncName "fastq") (ConstStr "fastq.fq") [] Nothing)])

case_type_complete = isOkTypesText
    "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \reads = unique(reads,max_copies=2)\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    read = substrim(read, min_quality=24)\n\
    \    if len(read) < 30:\n\
    \        discard\n"

case_indent_empty_line = isOkTypesText
    "ngless '0.0'\n\
    \reads = fastq('input1.fq')\n\
    \preprocess(reads) using |read|:\n\
    \    read = read[5:]\n\
    \    \n\
    \    if len(read) < 24:\n\
    \        discard\n"


case_invalid_func_fastq = isError $ checktypes [] f_expr
    where 
        f_expr = Script Nothing [(0,FunctionCall (FuncName "fastq") (ConstStr "fastq.fq") [(Variable "fname", (ConstNum 10))] Nothing)]

-- unique

case_valid_funique_mc = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \unique(x, max_copies=10)\n"

case_invalid_funique_mc = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \unique(x, max_copies='test')\n"


-- substrim

case_valid_fsubstrim_mq = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \preprocess(x) using |read|:\n\
    \    read = read[5:]\n\
    \    read = substrim(read, min_quality=2)\n"

case_invalid_fsubstrim_mq = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \preprocess(x) using |read|:\n\
    \    read = read[5:]\n\
    \    read = substrim(read, min_quality='2')\n"

-- map

case_invalid_fmap_ref = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \map(x, reference=10)\n"


case_valid_fmap_ref = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \map(x, reference='xpto')\n"

-- annotate
case_valid_fannot_gff = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, gff='xpto')"

case_invalid_fannot_gff = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, gff={xpto})"

case_valid_fannot_mode = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, mode={union})"

case_invalid_fannot_mode = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, mode='union')"

case_valid_fannot_features = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, features=[{gene}])"

case_invalid_fannot_features = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \annotate(y, features='gene')"

case_valid_fcount_min = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \k = count(z, min=10)"

case_invalid_fcount_min = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \k = count(z, min='10')"


case_valid_fcount_counts = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \k = count(z, counts=[{gene}])"

case_invalid_fcount_counts = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \k = count(z, counts=['gene'])"

-- write

case_valid_fwrite_ofile = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \write(count(z), ofile='10')"

case_invalid_fwrite_ofile = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \write(count(z), ofile=10)"


case_valid_fwrite_format = isOkTypesText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \write(count(z), format={tsv})"

case_invalid_fwrite_format = isErrorText
    "ngless '0.0'\n\
    \x = fastq('fq')\n\
    \y = map(x, reference='xpto')\n\
    \z = annotate(y, features=[{gene}])\n\
    \write(count(z), format='tsv')"
