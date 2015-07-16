{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
module Tests.Validation
    ( tgroup_Validation
    ) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Tests.Utils
import Validation
import ValidationNotPure
import Control.Monad

tgroup_Validation = $(testGroupGenerator)

case_mismatched_argument = isError $ (validate =<< parsetest "ngless '0.0'\n\
            \input = fastq('input.fq')\n\
            \mapped = map(input, reference='hg19')\n\
            \ann = annotate(mapped, mode={deny})")

validate_io_Ok script = do
    err <- validate_io (fromRight . parsetest $ script)
    case err of
        Nothing -> assertBool "" True
        Just errmsg -> assertFailure (concat ["Expected no errors in validation, got ", show errmsg, ".\nScript was::\n\n", show script])

validate_io_Error script = do
    err <- validate_io (fromRight . parsetest $ script)
    case err of
        Nothing -> assertFailure (concat ["ValidateIO should have detected an error on the script ", show script])
        Just _ -> return ()

validate_Error script =
    when (isRight $ parsetest script >>= validate) $
        assertFailure (concat ["Validate (pure) should have detected an error on the script ", show script])

isRight (Right _) = True
isRight (Left _) = False

case_fastq_inexistence_file = validate_io_Error s
    where s = "ngless '0.0'\n\
        \fastq('THIS_FILE_DOES_NOT_EXIST_SURELY.fq')\n"

case_invalid_not_pure_fp_fastq_lit = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \fastq('Makefile')\n" --File Makefile exists

case_valid_not_pure_fp_fastq_const = validate_io_Error s
    where s = "ngless '0.0'\n\
        \x = 'fq'\n\
        \fastq(x)\n"

case_invalid_not_pure_fp_fastq_const = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \x = 'Makefile'\n\
        \fastq(x)\n" --File Makefile always Exists


case_valid_not_pure_map_reference_lit = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \map(x, reference='Makefile')\n"

case_invalid_not_pure_map_def_reference_lit = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \map(x, reference='sacCer3')\n"

case_invalid_not_pure_map_reference_lit = validate_io_Error s
    where s = "ngless '0.0'\n\
        \map(x, reference='THIS_FILE_DOES_NOT_EXIST_SURELY.fa')\n"


case_valid_not_pure_map_reference_const = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \v = 'Makefile'\n\
        \map(x, reference=v)\n"

case_invalid_not_pure_map_def_reference_const = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \v = 'sacCer3'\n\
        \map(x, reference=v)\n"

case_invalid_not_pure_map_reference_const = validate_io_Error s
    where s = "ngless '0.0'\n\
        \v = 'THIS_FILE_DOES_NOT_EXIST_SURELY.fa'\n\
        \map(x, reference=v)\n"


case_valid_not_pure_annotate_gff_lit = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \annotate(x, gff='Makefile')\n"

case_invalid_not_pure_annotate_gff_lit = validate_io_Error s
    where s = "ngless '0.0'\n\
        \annotate(x, gff='THIS_FILE_DOES_NOT_EXIST_SURELY.gff')\n"


case_valid_not_pure_annotate_gff_const = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \v = 'Makefile'\n\
        \annotate(x, gff=v)\n"

case_invalid_not_pure_annotate_gff_const = validate_io_Error s
    where s = "ngless '0.0'\n\
        \v = 'THIS_FILE_DOES_NOT_EXIST_SURELY.gff'\n\
        \annotate(x, gff=v)\n"


case_valid_not_pure_annotate_gff_const2 = validate_io_Ok s
    where s = "ngless '0.0'\n\
        \v = 'fq'\n\
        \v = 'Makefile'\n\
        \annotate(x, gff=v)\n"

case_validate_internal_call = validate_Error
    "ngless '0.0'\n\
    \write(select(samfile('f.sam'), keep_if=[{matched}]), ofile=STDOUT)\n"
