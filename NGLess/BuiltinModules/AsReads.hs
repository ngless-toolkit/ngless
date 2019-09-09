{- Copyright 2015-2018 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module BuiltinModules.AsReads
    ( loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Algorithms.Async as CAlg
import Control.Monad.Trans.Resource (release)
import Data.Conduit ((.|))
import Control.Monad.Except
import System.IO
import Data.Default (def)
import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Concurrent (getNumCapabilities)

import Language
import FileManagement

import Interpretation.FastQ (encodingFor)

import Data.Sam
import Data.FastQ
import Modules
import Output
import NGLess
import FileOrStream
import Utils.Conduit (ByteLine(..))

executeReads :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeReads (NGOMappedReadSet name istream _) _ = NGOReadSet name <$> uncurry samToFastQ (asSamStream istream)
executeReads arg _ = throwShouldNotOccur ("executeReads called with argument: " ++ show arg)

executeDiscardSingles :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeDiscardSingles (NGOReadSet meta (ReadSet paired _)) [] = return (NGOReadSet meta (ReadSet paired []))
executeDiscardSingles arg _ = throwShouldNotOccur ("executeDiscardSingles called with argument: " ++ show arg)

data FQResult = NoResult
                | Single !B.ByteString
                | Paired !B.ByteString !B.ByteString
            deriving (Eq)

samToFastQ :: FilePath -> C.ConduitT () (V.Vector ByteLine) NGLessIO () -> NGLessIO ReadSet
samToFastQ fpsam stream = do
    (rk1, (oname1,ohand1)) <- openNGLTempFile' fpsam "reads_" "1.fq.gz"
    (rk2, (oname2,ohand2)) <- openNGLTempFile' fpsam "reads_" "2.fq.gz"
    (rk3, (oname3,ohand3)) <- openNGLTempFile' fpsam "reads_" "singles.fq.gz"
    hasPaired <- liftIO (newIORef False)
    hasSingle <- liftIO (newIORef False)
    let writer sel var out =
            CL.map sel
                .| do
                    empty <- CC.nullE
                    unless empty $
                        liftIO (writeIORef var True)
                    CC.concat .| CAlg.asyncGzipTo out
    numCapabilities <- liftIO getNumCapabilities
    [(),(),()] <- C.runConduit $
        stream
            .| readSamGroupsC' numCapabilities True
            .| CL.mapM (fmap (V.filter (/= NoResult)) . V.mapM asFQ)
            .| C.sequenceSinks
                [writer (V.mapMaybe (\r -> case r of Paired a _ -> Just a ; _ -> Nothing)) hasPaired ohand1
                ,writer (V.mapMaybe (\r -> case r of Paired _ b -> Just b ; _ -> Nothing)) hasPaired ohand2
                ,writer (V.mapMaybe (\r -> case r of Single a -> Just a; _ -> Nothing)) hasSingle ohand3
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


asFQ :: [SamLine] -> NGLessIO FQResult
asFQ sg = postproc (asFQ' False False . filter hasSequence $ sg)
    where
        postproc :: [(Int, B.ByteString)] -> NGLessIO FQResult
        postproc [(_,b)] = return $ Single b
        postproc [(1,a),(2,b)] = return $ Paired a b
        postproc [(2,b),(1,a)] = return $ Paired a b
        postproc [] = do
            outputListLno' WarningOutput ["No sequence information for read ", readID]
            return NoResult
        postproc other = throwShouldNotOccur ("Impossible argument to postproc: " ++ show other)
        readID = case sg of
            (f@SamLine{}:_) -> B8.unpack (samQName f)
            _ -> " [no read ID: this is likely a bug in ngless]"
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

discard_singles_Function = Function
    { funcName = FuncName "discard_singles"
    , funcArgType = Just NGLReadSet
    , funcArgChecks = []
    , funcRetType = NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension= True
    , funcChecks = [FunctionCheckMinNGLessVersion (1,1)
                   ,FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.as_reads" "0.0"
    , modFunctions = [as_reads_Function, discard_singles_Function]
    , runFunction = \case
                        "as_reads" -> executeReads
                        "discard_singles" -> executeDiscardSingles
                        _ -> error "NOT POSSIBLE"
    }

