{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Validation
    ( tgroup_Validation
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Text as T

import Tests.Utils
import Validation
import ValidationIO
import NGLess
import Control.Monad

tgroup_Validation = $(testGroupGenerator)

-- Pure Validation

isValidateOk ftext = case parsetest ftext >>= validate [] of
    Right _ -> return ()
    Left err -> assertFailure ("Validation should have passed for script "++T.unpack ftext++"; instead picked up error: '"++T.unpack err++"'")
isValidateError ftext = isErrorMsg ("Validation should have picked error for script '"++T.unpack ftext++"'") (parsetest ftext >>= validate [])

case_bad_function_attr_count = isValidateError
    "ngless '0.0'\n\
    \count(annotated, count={gene})\n"

case_good_function_attr_count_1 = isValidateOk
    "ngless '0.0'\n\
    \write(count(annotated, counts=[{gene}]),ofile='gene_counts.csv',format={csv})"

case_good_function_attr_count_2 = isValidateOk
    "ngless '0.0'\n\
    \counts = count(annotated, counts=[{gene}])"

case_map_not_assigned = isValidateError
    "ngless '0.0'\n\
    \map(input,reference='sacCer3')\n"

case_good_function_attr_map_1 = isValidateOk
    "ngless '0.0'\n\
    \write(map(input,reference='sacCer3'),ofile='result.sam',format={sam})"

case_good_function_attr_map_2 = isValidateOk
    "ngless '0.0'\n\
    \counts = map(input,reference='sacCer3')"

case_mismatched_argument = isValidateError
    "ngless '0.0'\n\
    \input = fastq('input.fq')\n\
    \mapped = map(input, reference='hg19')\n\
    \ann = annotate(mapped, mode={deny}"



-- Validate IO

validateIO_Ok script = do
    err <- testNGLessIO $ validateIO (fromRight . parsetest $ script)
    case err of
        Nothing -> assertBool "" True
        Just errmsg -> assertFailure (concat ["Expected no errors in validation, got ", show errmsg, ".\nScript was::\n\n", show script])

validateIO_error script = do
    err <- testNGLessIO $ validateIO (fromRight . parsetest $ script)
    case err of
        Nothing -> assertFailure (concat ["ValidateIO should have detected an error on the script ", show script])
        Just _ -> return ()

validate_Error script =
    when (isRight $ parsetest script >>= validate []) $
        assertFailure (concat ["Validate (pure) should have detected an error on the script ", show script])

isRight (Right _) = True
isRight (Left _) = False

case_fastq_inexistence_file = validateIO_error
    "ngless '0.0'\n\
    \fastq('THIS_FILE_DOES_NOT_EXIST_SURELY.fq')\n"

case_invalid_not_pure_fp_fastq_lit = validateIO_Ok
    "ngless '0.0'\n\
    \fastq('Makefile')\n" --File Makefile exists

case_valid_not_pure_fp_fastq_const = validateIO_error
    "ngless '0.0'\n\
    \x = 'fq'\n\
    \fastq(x)\n"

case_invalid_not_pure_fp_fastq_const = validateIO_Ok
    "ngless '0.0'\n\
    \x = 'Makefile'\n\
    \fastq(x)\n" --When run in source directory, Makefile exists


case_valid_not_pure_map_reference_lit = validateIO_Ok
    "ngless '0.0'\n\
    \map(x, reference='Makefile')\n"

case_invalid_not_pure_map_def_reference_lit = validateIO_Ok
    "ngless '0.0'\n\
    \map(x, reference='sacCer3')\n"

case_invalid_not_pure_map_reference_lit = validateIO_error
    "ngless '0.0'\n\
    \map(x, reference='THIS_FILE_DOES_NOT_EXIST_SURELY.fa')\n"


case_valid_not_pure_map_reference_const = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'Makefile'\n\
    \map(x, reference=v)\n"

case_invalid_not_pure_map_def_reference_const = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'sacCer3'\n\
    \map(x, reference=v)\n"

case_invalid_not_pure_map_reference_const = validateIO_error
    "ngless '0.0'\n\
    \v = 'THIS_FILE_DOES_NOT_EXIST_SURELY.fa'\n\
    \map(x, reference=v)\n"


case_valid_not_pure_annotate_gff_lit = validateIO_Ok
    "ngless '0.0'\n\
    \annotate(x, gff='Makefile')\n"

case_invalid_not_pure_annotate_gff_lit = validateIO_error
    "ngless '0.0'\n\
    \annotate(x, gff='THIS_FILE_DOES_NOT_EXIST_SURELY.gff')\n"

case_valid_not_pure_annotate_gff_const = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'Makefile'\n\
    \annotate(x, gff=v)\n"

case_invalid_not_pure_annotate_gff_const = validateIO_error
    "ngless '0.0'\n\
    \v = 'THIS_FILE_DOES_NOT_EXIST_SURELY.gff'\n\
    \annotate(x, gff=v)\n"


case_valid_not_pure_annotate_gff_const2 = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'fq'\n\
    \v = 'Makefile'\n\
    \annotate(x, gff=v)\n"

case_validate_internal_call = validate_Error
    "ngless '0.0'\n\
    \write(select(samfile('f.sam'), keep_if=[{matched}]), ofile=STDOUT)\n"
