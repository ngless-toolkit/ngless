{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Mocat
    ( loadModule
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit ((=$=), ($$))
import           System.IO
import System.FilePath
import System.FilePath.Glob
import Data.String.Utils
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Applicative
import Data.Default

import Output
import NGLess
import Modules
import Language
import FileManagement
import Utils.Conduit

dropEnd :: Int -> [a] -> [a]
dropEnd v a = take (length a - v) a -- take of a negative is the empty sequence, which is correct in this case

replaceEnd :: String -> String -> String -> Maybe String
replaceEnd end newEnd str
        | endswith end str = Just (dropEnd (length end) str ++ newEnd)
        | otherwise = Nothing

mocatSamplePaired :: [FilePath] -> NGLessIO [Expression]
mocatSamplePaired matched = do
    let matched1 = filter (\f -> endswith ".1.fq.gz" f || endswith "1.fq.bz2" f) matched
    forM matched1 $ \m1 -> do
        let Just m2 = replaceEnd "1.fq.gz" "2.fq.gz" m1 <|> replaceEnd "1.fq.bz2" "2.fq.bz2" m1
        unless (m2 `elem` matched) $
            throwDataError ("Cannot find match for file: " ++ m1)
        return (FunctionCall (FuncName "paired") (ConstStr . T.pack $ m1) [(Variable "second", ConstStr . T.pack $ m2)] Nothing)


executeLoad :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoad (NGOString samplename) [] = NGOExpression <$> do
    outputListLno' TraceOutput ["Executing mocat_load_sample transform"]
    let basedir = T.unpack samplename
    matched <- liftIO $ liftM2 (++)
                (namesMatching (basedir </> "*.fq.gz"))
                (namesMatching (basedir </> "*.fq.bz2"))
    let matched1 = filter (\f -> endswith ".1.fq.gz" f || endswith "1.fq.bz2" f) matched
    args <- ListExpression <$> if null matched1
            then return [FunctionCall (FuncName "fastq") (ConstStr . T.pack $ f) [] Nothing | f <- matched]
            else mocatSamplePaired matched
    return (FunctionCall (FuncName "group") args [(Variable "name", ConstStr samplename)] Nothing)
executeLoad _ _ = throwShouldNotOccur "mocat_load_sample got the wrong arguments."

executeParseCoord :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeParseCoord (NGOString coordfp) _ = do
    let coordfp' = T.unpack coordfp
    (newfp, hout) <- openNGLTempFile coordfp' "converted_" ".gtf"
    conduitPossiblyCompressedFile coordfp'
        =$= CB.lines
        =$= CL.mapM convertToGff
        =$= C.unlinesAscii
        $$ C.sinkHandle hout
    liftIO $ hClose hout
    return (NGOString . T.pack $ newfp)
        where
            convertToGff :: B.ByteString -> NGLessIO B.ByteString
            convertToGff line = case B8.split '\t' line of
                                    [gene,start,end] -> return $! B.intercalate "\t"
                                                                    [gene
                                                                    ,"protein_coding"
                                                                    ,"gene"
                                                                    ,start
                                                                    ,end
                                                                    ,"."
                                                                    ,"."
                                                                    ,"."
                                                                    ,B.concat ["gene_id=\"", gene, "\""]
                                                                    ]
                                    _ -> throwDataError ("Could not parse coord file entry: " ++ show line)
executeParseCoord other _ = throwScriptError ("coord_file_to_gtf called with wrong argument: " ++ show other)

mocatLoadSample = Function
    { funcName = FuncName "load_mocat_sample"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }

coordToGtf = Function
    { funcName = FuncName "coord_file_to_gtf"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }


loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.mocat" "0.0"
        , modCitation = Just citation
        , modFunctions = [mocatLoadSample, coordToGtf]
        , runFunction = \case
                        "load_mocat_sample" -> executeLoad
                        "coord_file_to_gtf" -> executeParseCoord
                        other -> \_ _ -> throwShouldNotOccur ("mocat execute function called with wrong arguments: " ++ show other)
        }
    where
        citation = T.concat
            ["MOCAT2: a metagenomic assembly, annotation and profiling framework.\n"
            ,"Kultima JR, Coelho LP, Forslund K, Huerta-Cepas J, Li S, Driessen M, et al. (2016)\n"
            ,"Bioinformatics (2016) doi:10.1093/bioinformatics/btw183\n\n"
            ,"MOCAT: A Metagenomics Assembly and Gene Prediction Toolkit.\n"
            ,"Kultima JR, Sunagawa S, Li J, Chen W, Chen H, Mende DR, et al. (2012)\n"
            ,"PLoS ONE 7(10): e47656. doi:10.1371/journal.pone.0047656\n"]
