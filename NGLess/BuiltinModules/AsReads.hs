{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.AsReads
    ( executeReads
    , loadModule
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Zlib as C
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($=), ($$), (=$=), (=$))
import Data.Conduit.Async ((=$=&), ($$&))
import Data.Function (on)
import Control.Monad.Except
import Control.Exception
import System.IO

import Language
import FileManagement

import Data.Sam
import Data.FastQ
import Modules
import NGLess

executeReads :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReads (NGOMappedReadSet fpsam _) _ = do
    fp <- samToFastQ fpsam
    return (NGOReadSet $ ReadSet1 SangerEncoding fp)
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

samToFastQ :: FilePath -> NGLessIO FilePath
samToFastQ fpsam = do
    (oname,ohand) <- openNGLTempFile fpsam "reads_" "fq"
    C.sourceFile fpsam
        $= CB.lines
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

readSamGroupsC :: C.Conduit B.ByteString NGLessIO [SamLine]
readSamGroupsC = readSamLineOrDie =$= CL.groupBy groupLine
    where
        readSamLineOrDie = C.awaitForever $ \line ->
            case readSamLine (BL.fromStrict line) of
                Left err -> throwError err
                Right parsed@SamLine{} -> C.yield parsed
                _ -> return ()
        groupLine = (==) `on` samQName

as_reads_Function = Function
    { funcName = FuncName "as_reads"
    , funcArgType = Just NGLMappedReadSet
    , funcRetType = NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = True
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return Module
    { modInfo = ModInfo "builtin.as_reads" "0.0"
    , modConstants = []
    , modFunctions = [as_reads_Function]
    , runFunction = const executeReads
    , validateFunction = const (return [])
    }

