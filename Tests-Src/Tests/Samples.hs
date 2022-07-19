{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{- Copyright 2022 NGLess Authors
 - License: MIT
 -}
module Tests.Samples
    ( tgroup_Samples
    ) where

import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)

import Data.FastQ
import BuiltinModules.Samples
import Language
import Utils.Here
import Tests.Utils (asTempFile, testNGLessIO)

simpleYaml :: B.ByteString
simpleYaml = [here|
samples:
  sample1:
    - paired:
        - sample/sample1a.1.fq.gz
        - sample/sample1a.2.fq.gz
  sample2:
    - paired:
        - sample/sample2a.1.fq.gz
        - sample/sample2a.2.fq.gz
|]

yamlWithBasedir :: B.ByteString
yamlWithBasedir = [here|
basedir: /share/metagenomes
samples:
  sample1:
    - paired:
        - sample/sample1a.1.fq.gz
        - sample/sample1a.2.fq.gz
  sample2:
    - paired:
        - /share/data/sample/sample2a.1.fq.gz
        - /share/data/sample/sample2a.2.fq.gz
|]

tgroup_Samples = $(testGroupGenerator)

getSampleName (NGOReadSet name _) = name

case_load_samples :: Assertion
case_load_samples = testNGLessIO $ do
    simpleYamlF <- asTempFile simpleYaml "yaml"
    NGOList samples <- executeLoadSampleList (NGOString $ T.pack simpleYamlF) []
    liftIO $ length samples @?= 2
    liftIO $ (getSampleName $ head samples) @?= "sample1"
    liftIO $ (getSampleName $ last samples) @?= "sample2"
    NGOReadSet n _ <- executeLoadSample (NGOString $ T.pack simpleYamlF) [("sample", NGOString "sample1")]
    liftIO $ n @?= "sample1"


case_basedir :: Assertion
case_basedir = do
    NGOList samples <- testNGLessIO $ do
        simpleYamlF <- asTempFile yamlWithBasedir "yaml"
        executeLoadSampleList (NGOString $ T.pack simpleYamlF) []
    let getSamplePaths (NGOReadSet _ (ReadSet [
                                        (FastQFilePath _ p1
                                        ,FastQFilePath _ p2)]
                                        [])) = (p1, p2)
        getSamplePaths _ = error "should not occur"
    length samples @?= 2
    getSamplePaths (head samples) @?= (
                    "/share/metagenomes/sample/sample1a.1.fq.gz",
                    "/share/metagenomes/sample/sample1a.2.fq.gz")
    getSamplePaths (last samples) @?= (
                    "/share/data/sample/sample2a.1.fq.gz",
                    "/share/data/sample/sample2a.2.fq.gz")
