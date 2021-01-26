{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Tests.LoadFQDirectory
    ( tgroup_LoadFQDirectory
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Control.Monad (forM_)

import BuiltinModules.LoadDirectory (matchUp)
import Tests.Utils

tgroup_LoadFQDirectory = $(testGroupGenerator)

case_error_p1 = isError (matchUp ["sample/sample.pair.1.fq.bz2"])
case_error_p2 = isError (matchUp ["sample/sample.pair.2.fq.bz2"])

case_known_cases = forM_ known_cases check1

check1 expected@(s, p) = do
    let extract (Left (a,b)) = [a,b]
        extract (Right (a,b,c)) = [a,b,c]
        fqs = s ++ concatMap extract p
    fromRight (matchUp fqs) @?= expected

known_cases = 
    --  Simple with ".single"
    [ ([], [Right ("sample/sample.pair.1.fq", "sample/sample.pair.2.fq", "sample/sample.single.fq")])

    -- basic uncompressed
    , (["sample/uncompressed.fq"], [])

    -- basic compressed I
    , (["sample/sample.fq.gz"], [])

    -- basic compressed II
    , (["sample/sample.fq.bz2"], [])

    -- Paired using "_[12].fastq"
    , ([], [Left ("sample/sample_1.fastq.gz","sample/sample_2.fastq.gz")])

    -- paired+singles compressed
    , ([], [Right ("sample/sample.pair.1.fq.gz", "sample/sample.pair.2.fq.gz", "sample/sample.single.fq.gz")])

    -- paired+singles compressed II (bz2 + deeper directory)
    , ([], [Right ("mocat_sample_bz2_paired/sample/sample.pair.1.fq.bz2", "mocat_sample_bz2_paired/sample/sample.pair.2.fq.bz2", "mocat_sample_bz2_paired/sample/sample.single.fq.bz2")])

    -- _[FR] pairing+single
    -- Arguably, the pairing is not the expected one, but this was what was done before
    , (["sample/sample.single.fq.gz"], [Left ("sample/sample.pair_F.fq.gz", "sample/sample.pair_R.fq.gz")])

    -- mixed
    , (["sample/sampleC.single.fq.bz2"],
            [Left  ("sample/sampleB.pair.1.fq.bz2", "sample/sampleB.pair.2.fq.bz2")
            ,Right ("sample/sampleA.pair.1.fq.bz2", "sample/sampleA.pair.2.fq.bz2", "sample/sampleA.single.fq.bz2")])
    ]

