{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.AsReads
    ( executeReads
    , loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($=), (=$=), ($$))
import Control.Monad.Except
import System.IO
import Data.Default

import Language
import FileManagement

import Data.Sam
import Data.FastQ
import Modules
import Output
import NGLess
import Utils.Conduit

executeReads :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReads (NGOMappedReadSet name fpsam _) _ = NGOReadSet name <$> samToFastQ fpsam
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

samToFastQ :: FilePath -> NGLessIO ReadSet
samToFastQ fpsam = do
    (oname1,ohand1) <- openNGLTempFile fpsam "reads_" ".1.fq"
    (oname2,ohand2) <- openNGLTempFile fpsam "reads_" ".2.fq"
    (oname3,ohand3) <- openNGLTempFile fpsam "reads_" ".singles.fq"
    C.sourceFile fpsam
        $= linesC
        =$= readSamGroupsC
        =$= CL.map asFQ
        $$ CL.mapM_ (liftIO . \case
            Left r -> B.hPut ohand3 r
            Right (r1,r2) -> B.hPut ohand1 r1 >> B.hPut ohand2 r2)
    outputListLno' TraceOutput ["Finished as_reads"]
    liftIO $ forM_ [ohand1, ohand2, ohand3] hClose
    return $! ReadSet3 SolexaEncoding oname1 oname2 oname3


asFQ :: [SamLine] -> Either B.ByteString (B.ByteString,B.ByteString)
asFQ = postproc . asFQ' False False
    where
        postproc [(_,b)] = Left b
        postproc [(1,a),(2,b)] = Right (a,b)
        postproc [(2,b),(1,a)] = Right (a,b)
        postproc other = error ("Impossible argument to postproc: " ++ show other)
        asFQ'  False False [s] = [(3 :: Int, asFQ1 s)]
        asFQ'  _ _ []= []
        asFQ' seen1 seen2 (s:ss)
            | isFirstInPair  s && not seen1 = (1 :: Int, asFQ1 s):asFQ' True seen2 ss
            | isSecondInPair s && not seen2 = (2 :: Int, asFQ1 s):asFQ' seen1 True ss
            | otherwise = asFQ' seen1 seen2 ss
        asFQ1 SamLine{samQName=qname, samSeq=short, samQual=qs} = B.concat ["@", qname, "\n", short, "\n+\n", qs, "\n"]
        asFQ1 SamHeader{} = error "Should not have seen a header in this place"


as_reads_Function = Function
    { funcName = FuncName "as_reads"
    , funcArgType = Just NGLMappedReadSet
    , funcRetType = NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = True
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.as_reads" "0.0"
    , modFunctions = [as_reads_Function]
    , runFunction = const executeReads
    }

