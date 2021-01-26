{- Copyright 2016-2020 NGLess Authors
 - License: MIT
 -}


module StandardModules.Mocat
    ( loadModule
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import Control.Monad
import Data.Default


import Output
import NGLess
import Modules
import Language
import FileManagement
import Utils.Conduit
import NGLess.NGLEnvironment
import BuiltinModules.LoadDirectory (executeLoad)

executeLoad' :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoad' expr kwargs = do
    v <- ngleVersion <$> nglEnvironment
    when (v >= NGLVersion 1 2) $
        outputListLno' InfoOutput [
                        "load_mocat_sample is now available as load_fastq_directory without the need to load the 'mocat' module"]
    executeLoad expr kwargs


executeParseCoord :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeParseCoord (NGOString coordfp) _ = do
    let coordfp' = T.unpack coordfp
    newfp <- makeNGLTempFile coordfp' "converted_" "gtf" $ \hout ->
        C.runConduit $
            conduitPossiblyCompressedFile coordfp'
                .| linesC
                .| CL.mapM convertToGff
                .| C.unlinesAscii
                .| C.sinkHandle hout
    return (NGOString . T.pack $ newfp)
        where
            convertToGff :: ByteLine -> NGLessIO B.ByteString
            convertToGff (ByteLine line) = case B8.split '\t' line of
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
    , funcKwArgs =
            [ArgInformation "__perform_qc" False NGLBool []
            ,ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
            ]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

coordToGtf = Function
    { funcName = FuncName "coord_file_to_gtf"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }


loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.mocat" "1.1"
        , modCitations = citations
        , modFunctions = [mocatLoadSample, coordToGtf]
        , runFunction = \case
                        "load_mocat_sample" -> executeLoad'
                        "coord_file_to_gtf" -> executeParseCoord
                        other -> \_ _ -> throwShouldNotOccur ("mocat execute function called with wrong arguments: " ++ show other)
        }
    where
        citations =
            [T.concat ["MOCAT2: a metagenomic assembly, annotation and profiling framework.\n"
            ,"Kultima JR, Coelho LP, Forslund K, Huerta-Cepas J, Li S, Driessen M, et al. (2016)\n"
            ,"Bioinformatics (2016) doi:10.1093/bioinformatics/btw183\n\n"]
            ,T.concat ["MOCAT: A Metagenomics Assembly and Gene Prediction Toolkit.\n"
            ,"Kultima JR, Sunagawa S, Li J, Chen W, Chen H, Mende DR, et al. (2012)\n"
            ,"PLoS ONE 7(10): e47656. doi:10.1371/journal.pone.0047656\n"]]
