{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module BuiltinModules.AsReads
    ( executeReads
    , loadModule
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
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
    return (NGOReadSet1 SangerEncoding fp)
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

samToFastQ :: FilePath -> NGLessIO FilePath
samToFastQ fpsam = do
    (oname,ohand) <- openNGLTempFile fpsam "reads_" "fq"
    liftIO $ do
        fq <-  (map asFQ . readAlignments) <$> BL.readFile fpsam
        BL.hPut ohand (BL.concat fq)
        hClose ohand
        return oname

asFQ :: SamLine -> BL.ByteString
asFQ SamLine{samQName=qname, samSeq=short, samQual=qs} = BL.fromChunks ["@", qname, "\n", short, "\n+\n", qs, "\n"]

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

