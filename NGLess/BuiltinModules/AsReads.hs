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
import NGLess
import Utils.Conduit

executeReads :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReads (NGOMappedReadSet name fpsam _) _ = do
    fp <- samToFastQ fpsam
    return (NGOReadSet name $ ReadSet1 SangerEncoding fp)
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

samToFastQ :: FilePath -> NGLessIO FilePath
samToFastQ fpsam = do
    (oname,ohand) <- openNGLTempFile fpsam "reads_" "fq"
    C.sourceFile fpsam
        $= linesC
        =$= readSamGroupsC
        =$= CL.map asFQ
        =$= CL.concat
        $$ CB.sinkHandle ohand
    liftIO (hClose ohand)
    return oname

asFQ :: [SamLine] -> [B.ByteString]
asFQ = asFQ' False False
    where
        asFQ'  _ _ []= []
        asFQ' seen1 seen2 (s:ss)
            | isPositive s && (not seen1) = asFQ1 s:asFQ' True seen2 ss
            | isNegative s && (not seen2) = asFQ1 s:asFQ' seen1 True ss
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

