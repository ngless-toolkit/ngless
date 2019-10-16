{-# LANGUAGE MultiWayIf #-}
{- Copyright 2016-2019 NGLess Authors
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
import           Control.Monad.Extra (unlessM)
import           System.Directory (doesDirectoryExist)
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
    (s1,s2) <- [(".1", ".2") ,("_1", "_2"), ("_F", "_R")]
    return (s1 ++ "." ++ end, s2 ++ "." ++ end)

buildSingle m1
    | "pair.1" `isInfixOf` m1 = replace "pair.1" "single" m1
    | "pair.2" `isInfixOf` m1 = replace "pair.2" "single" m1
    | otherwise = "MARKER_FOR_FILE_WHICH_DOES_NOT_EXIST"

mocatSamplePaired :: [FilePath] -> T.Text -> Bool -> NGLessIO [Expression]
mocatSamplePaired fqfiles encoding doQC = do
    let passthru = [(Variable "__perform_qc", ConstBool doQC), (Variable "encoding", ConstSymbol encoding)]
        match1 :: FilePath -> Maybe (FilePath, FilePath)
        match1 fp = listToMaybe . flip mapMaybe pairedEnds $ \(p1,p2) -> if
                        | (endswith p1 fp) -> Just (fp, dropEnd (length p1) fp ++ p2)
                        | (endswith p2 fp) -> Just (dropEnd (length p2) fp ++ p1, fp)
                        | otherwise -> Nothing
        -- match1 returns repeated entries if both pair.1 and pair.2 exist, uniq removes duplicate records
        matched1 = uniq $ mapMaybe match1 fqfiles
        encodeStr = ConstStr . T.pack
    (exps,used) <- fmap unzip $ forM matched1 $ \(m1,m2) -> do
        let singles = buildSingle m1
        unless (m1 `elem` fqfiles) $ throwDataError ("Cannot find match for file: " ++ m2)
        unless (m2 `elem` fqfiles) $ throwDataError ("Cannot find match for file: " ++ m1)
        let singlesArgs
                | singles `elem` fqfiles = (Variable "singles", encodeStr singles):passthru
                | otherwise = passthru
        outputListLno' InfoOutput ["load_mocat_sample found paired-end sample '", m1, "' - '", m2, if singles `elem` fqfiles then "' with singles file '" ++ singles ++ "'" else "'"]
        let expr = FunctionCall (FuncName "paired") (encodeStr m1) ((Variable "second", encodeStr m2):singlesArgs) Nothing
            used = [m1, m2, singles]
        return (expr, used)
    let singletonFiles = [f | f <- fqfiles, f `notElem` concat used]
        singletons = [FunctionCall (FuncName "fastq") (encodeStr f) passthru Nothing
                                | f <- singletonFiles]
    forM_ singletonFiles $ \f ->
        outputListLno' InfoOutput ["load_mocat_sample found single-end sample '", f, "'"]
    return $ singletons ++ exps


executeLoad :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoad (NGOString samplename) kwargs = NGOExpression <$> do
    qcNeeded <- lookupBoolOrScriptErrorDef (return True) "hidden QC argument" "__perform_qc" kwargs
    encoding <- lookupSymbolOrScriptErrorDef (return "auto") "encoding passthru argument" "encoding" kwargs
    outputListLno' TraceOutput ["Executing load_mocat_sample transform"]
    let basedir = T.unpack samplename
    unlessM (liftIO $ doesDirectoryExist basedir) $
        throwDataError ("Attempting to load directory '"++basedir++"', but directory does not exist.")
    fqfiles <- fmap (sort . concat) $ forM exts $ \pat ->
        liftIO $ namesMatching (basedir </> ("*." ++ pat))
    args <- mocatSamplePaired fqfiles encoding qcNeeded
    return (FunctionCall (FuncName "group") (ListExpression args) [(Variable "name", ConstStr samplename)] Nothing)
executeLoad _ _ = throwShouldNotOccur "load_mocat_sample got the wrong arguments."

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
        { modInfo = ModInfo "stdlib.mocat" "1.0"
        , modCitations = citations
        , modFunctions = [mocatLoadSample, coordToGtf]
        , runFunction = \case
                        "load_mocat_sample" -> executeLoad
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
