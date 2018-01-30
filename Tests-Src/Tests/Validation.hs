{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.Validation
    ( tgroup_Validation
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import Data.Either.Combinators (isRight)
import qualified Data.Text as T

import Tests.Utils
import Validation
import ValidationIO
import Utils.Here

tgroup_Validation = $(testGroupGenerator)

-- Pure Validation

isValidateOk ftext = case parsetest ftext >>= validate [] of
    Right _ -> return ()
    Left err -> assertFailure ("Validation should have passed for script "++T.unpack ftext++"; instead picked up error: '"++show err++"'")
isValidateError ftext = isErrorMsg ("Validation should have picked error for script '"++T.unpack ftext++"'") (parsetest ftext >>= validate [])

case_bad_function_attr_count = isValidateError
    "ngless '0.0'\n\
    \count(annotated, features='gene')\n"

case_map_not_assigned = isValidateError
    [here|
ngless '0.0'
input = fasq('input.fq.gz')
map(input,reference='sacCer3')
|]

case_good_function_attr_map = isValidateOk
    [here|
ngless '0.0'
input = fastq('input.fq.gz')
write(
    map(input, reference='sacCer3'),
            ofile='result.sam',
            format={sam})
|]


-- Validate IO

validateIO_Ok script = do
    err <- testNGLessIO $ validateIO [] (fromRight . parsetest $ script)
    case err of
        Nothing -> assertBool "" True
        Just errmsg -> assertFailure (concat ["Expected no errors in validation, got ", show errmsg, ".\nScript was::\n\n", show script])

validateIO_error script = do
    err <- testNGLessIO $ validateIO [] (fromRight . parsetest $ script)
    case err of
        Nothing -> assertFailure (concat ["ValidateIO should have detected an error on the script ", show script])
        Just _ -> return ()

validate_Error script =
    when (isRight $ parsetest script >>= validate []) $
        assertFailure (concat ["Validate (pure) should have detected an error on the script ", show script])

case_fastq_inexistence_file = validateIO_error
    "ngless '0.0'\n\
    \fastq('THIS_FILE_DOES_NOT_EXIST_SURELY.fq')\n"

case_invalid_not_pure_fp_fastq_lit = validateIO_Ok
    "ngless '0.0'\n\
    \fastq('Makefile')\n" --File Makefile exists

case_build_path = validateIO_Ok [here|
ngless '0.0'
part1 = 'Make'
part2 = 'file'
fastq(part1 + part2)
|]

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
    \map(x, fafile='Makefile')\n"

case_invalid_not_pure_map_def_reference_lit = validateIO_Ok
    "ngless '0.0'\n\
    \map(x, reference='sacCer3')\n"

case_invalid_not_pure_map_reference_lit = validateIO_error
    "ngless '0.0'\n\
    \map(x, fafile='THIS_FILE_DOES_NOT_EXIST_SURELY.fa')\n"

case_inexistent_reference = validateIO_error
    "ngless '0.0'\n\
    \map(x, reference='UNKNOWN_REFERENCE')\n"


case_fafile_through_variable = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'Makefile'\n\
    \map(x, fafile=v)\n"

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
    \count(x, gff_file='Makefile')\n"

case_invalid_not_pure_annotate_gff_lit = validateIO_error [here|
ngless '0.0'
count(x, gff_file='THIS_FILE_DOES_NOT_EXIST_SURELY.gff')
|]

case_samfile_check_file = validateIO_error [here|
ngless '0.0'
mapped = samfile('THIS_FILE_DOES_NOT_EXIST_SURELY.sam')
write(mapped, ofile='copy.sam')
|]

case_valid_not_pure_annotate_gff_const = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'Makefile'\n\
    \count(x, gff_file=v)\n"

case_invalid_not_pure_annotate_gff_const = validateIO_error
    "ngless '0.0'\n\
    \v = 'THIS_FILE_DOES_NOT_EXIST_SURELY.gff'\n\
    \count(x, gff_file=v)\n"


case_valid_not_pure_annotate_gff_const2 = validateIO_Ok
    "ngless '0.0'\n\
    \v = 'fq'\n\
    \v = 'Makefile'\n\
    \count(x, gff_file=v)\n"

case_validate_internal_call = validate_Error
    "ngless '0.0'\n\
    \write(select(samfile('f.sam'), keep_if=[{matched}]), ofile=STDOUT)\n"

case_validate_no_assign_constant = isValidateError [here|
ngless '0.0'
CONST = 1
CONST = 2
|]

case_validate_assign_variable = isValidateOk [here|
ngless '0.0'
notConst = 1
notConst = 2
|]
