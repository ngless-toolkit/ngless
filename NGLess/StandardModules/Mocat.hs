{- Copyright 2016-2017 NGLess Authors
 - License: MIT
 -}


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
import Data.List.Utils
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Maybe
import Data.Default
import Data.List (sort, isInfixOf)

import Output
import NGLess
import Modules
import Language
import FileManagement
import Utils.Conduit
import Utils.Utils (dropEnd)

exts :: [FilePath]
exts = do
    fq <- ["fq", "fastq"]
    comp <- ["", ".gz", ".bz2"]
    return $! fq ++ comp

pairedEnds :: [(String, String)]
pairedEnds = do
    end <- exts
    (s1,s2) <- [(".1", ".2") ,("_1", "_2")]
    return (s1 ++ "." ++ end, s2 ++ "." ++ end)

buildSingle m1
    | "pair.1" `isInfixOf` m1 = replace "pair.1" "single" m1
    | otherwise = "MARKER_FOR_FILE_WHICH_DOES_NOT_EXIST"

mocatSamplePaired :: [FilePath] -> T.Text -> Bool -> NGLessIO [Expression]
mocatSamplePaired fqfiles encoding doQC = do
    let passthru = [(Variable "__perform_qc", ConstBool doQC), (Variable "encoding", ConstSymbol encoding)]
        match1 :: FilePath -> Maybe (FilePath, FilePath)
        match1 fp = listToMaybe . flip mapMaybe pairedEnds $ \(p1,p2) -> do
                        guard (endswith p1 fp)
                        return (fp, dropEnd (length p1) fp ++ p2)
        matched1 = mapMaybe match1 fqfiles
        encodeStr = ConstStr . T.pack
    (exps,used) <- fmap unzip $ forM matched1 $ \(m1,m2) -> do
        let singles = buildSingle m1
        unless (m2 `elem` fqfiles) $
            throwDataError ("Cannot find match for file: " ++ m1)
        let singlesArgs
                | singles `elem` fqfiles = (Variable "singles", encodeStr singles):passthru
                | otherwise = passthru
        outputListLno' DebugOutput ["mocat_load_sample found ", m1, " # ", m2, if singles `elem` fqfiles then " # " ++ singles else ""]
        let expr = FunctionCall (FuncName "paired") (encodeStr m1) ((Variable "second", encodeStr m2):singlesArgs) Nothing
            used = [m1, m2, singles]
        return (expr, used)
    let singletons = [FunctionCall (FuncName "fastq") (encodeStr f) passthru Nothing
                                | f <- fqfiles, f `notElem` concat used]
    return $ singletons ++ exps


executeLoad :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoad (NGOString samplename) kwargs = NGOExpression <$> do
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "hidden QC argument" "__perform_qc" kwargs
    encoding <- lookupSymbolOrScriptErrorDef (return "auto") "encoding passthru argument" "encoding" kwargs
    outputListLno' TraceOutput ["Executing mocat_load_sample transform"]
    let basedir = T.unpack samplename
    fqfiles <- fmap (sort . concat) $ forM exts $ \pat ->
        liftIO $ namesMatching (basedir </> ("*." ++ pat))
    args <- mocatSamplePaired fqfiles encoding qcNeeded
    return (FunctionCall (FuncName "group") (ListExpression args) [(Variable "name", ConstStr samplename)] Nothing)
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
    , funcKwArgs =
            [ArgInformation "__perform_qc" False NGLBool []
            ,ArgInformation "encoding" False NGLSymbol [ArgCheckSymbol ["auto", "33", "64", "sanger", "solexa"]]
            ]
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
