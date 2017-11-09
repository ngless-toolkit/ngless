{- Copyright 2015-2017 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts #-}

module BuiltinModules.AsReads
    ( loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (release)
import Data.Conduit ((=$=), ($$))
import Control.Monad.Except
import System.IO
import Data.Default
import Data.IORef
import Data.Either.Combinators

import Language
import FileManagement

import Interpretation.FastQ (encodingFor)

import Data.Sam
import Data.FastQ
import Modules
import Output
import NGLess
import FileOrStream
import Utils.Conduit

executeReads :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReads (NGOMappedReadSet name istream _) _ = NGOReadSet name <$> uncurry samToFastQ (asSamStream istream)
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

samToFastQ :: FilePath -> C.Source NGLessIO ByteLine -> NGLessIO ReadSet
samToFastQ fpsam stream = do
    (rk1, (oname1,ohand1)) <- openNGLTempFile' fpsam "reads_" ".1.fq.gz"
    (rk2, (oname2,ohand2)) <- openNGLTempFile' fpsam "reads_" ".2.fq.gz"
    (rk3, (oname3,ohand3)) <- openNGLTempFile' fpsam "reads_" ".singles.fq.gz"
    hasPaired <- liftIO (newIORef False)
    hasSingle <- liftIO (newIORef False)
    let writer sel var out =
            CL.mapMaybe sel
                =$= (do
                        awaitJust $ \val -> do
                            liftIO (writeIORef var True)
                            C.yield val
                        C.awaitForever C.yield)
                =$= asyncGzipTo out
    void $
        stream
        =$= readSamGroupsC
        =$= CL.map asFQ
        $$ C.sequenceSinks
            [writer (liftM fst . rightToMaybe)  hasPaired ohand1
            ,writer (liftM snd . rightToMaybe)  hasPaired ohand2
            ,writer leftToMaybe                 hasSingle ohand3
            ]
    outputListLno' TraceOutput ["Finished as_reads"]
    liftIO $ forM_ [ohand1, ohand2, ohand3] hClose
    hasPaired' <- liftIO $ readIORef hasPaired
    hasSingle' <- liftIO $ readIORef hasSingle
    case (hasPaired', hasSingle') of
        (True, True) -> do
            enc <- encodingFor oname1
            return $! ReadSet [(FastQFilePath enc oname1,FastQFilePath enc oname2)] [FastQFilePath enc oname3]
        (False, True) -> do
            release rk1
            release rk2
            enc <- encodingFor oname3
            return $! ReadSet [] [FastQFilePath enc oname3]
        (True, False) -> do
            release rk3
            enc <- encodingFor oname1
            return $! ReadSet [(FastQFilePath enc oname1,FastQFilePath enc oname2)] []
        (False, False) -> do
            -- the input is empty
            release rk3
            outputListLno' WarningOutput ["as_reads returning an empty read set"]
            return $! ReadSet [(FastQFilePath SangerEncoding oname1,FastQFilePath SangerEncoding oname2)] []


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
    , funcArgChecks = []
    , funcRetType = NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = True
    , funcChecks = [FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.as_reads" "0.0"
    , modFunctions = [as_reads_Function]
    , runFunction = const executeReads
    }

