{- Copyright 2018-2019 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE RankNTypes #-}

module StandardModules.Mappers.Minimap2
    ( hasValidIndex
    , createIndex
    , callMapper
    ) where

import           System.Process (proc, readProcessWithExitCode)
import           System.Exit (ExitCode(..))
import           System.Directory (doesFileExist)
import           System.Path (splitExt)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit as C
import qualified UnliftIO as U
import           Data.Conduit ((.|))
import           Data.Conduit.Algorithms (mergeC)
import           GHC.Conc (getNumCapabilities, setNumCapabilities)
import           Control.Monad.Trans.Class (lift)

import Data.FastQ
import Data.Sam (isSamHeaderString)
import Output
import NGLess
import Configuration
import Dependencies.Versions (minimap2Version)
import NGLess.NGLEnvironment
import Utils.Vector (sortParallel)
import Utils.Conduit (linesC, ByteLine(..))
import FileManagement (makeNGLTempFile, minimap2Bin)

indexName :: FilePath -> FilePath
indexName fp = base ++ "-minimap2-" ++ minimap2Version ++ ext ++ ".mm2.idx"
    where (base, ext) = splitExt fp

hasValidIndex :: FilePath -> NGLessIO Bool
hasValidIndex = liftIO . doesFileExist . indexName

createIndex :: FilePath -> NGLessIO ()
createIndex fafile = do
    outputListLno' InfoOutput ["Start minimap2 index creation for ", fafile]
    minimap2Path <- minimap2Bin
    (exitCode, out, err) <- liftIO $
        readProcessWithExitCode minimap2Path [fafile, "-d", indexName fafile] []
    outputListLno' DebugOutput ["minimap2-index stderr: ", err]
    outputListLno' DebugOutput ["minimap2-index stdout: ", out]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _err -> throwSystemError err

callMapper :: FilePath -> ReadSet -> [String] -> C.ConduitT B.ByteString C.Void NGLessIO a -> NGLessIO a
callMapper refIndex rs extraArgs outC = do
    outputListLno' InfoOutput ["Starting mapping to ", refIndex, " (minimap2)"]
    minimap2Path <- minimap2Bin
    numCapabilities <- liftIO getNumCapabilities
    strictThreads <- nConfStrictThreads <$> nglConfiguration
    let minimap2threads
            | strictThreads && numCapabilities > 1 = numCapabilities - 1
            | otherwise = numCapabilities
        with1Thread act
            | strictThreads = U.bracket_
                                (liftIO $ setNumCapabilities 1)
                                (liftIO $ setNumCapabilities numCapabilities)
                                act
            | otherwise = act
        cmdargs =  concat [["-t", show minimap2threads, "-a"], extraArgs, [refIndex, "-"]]
    outputListLno' TraceOutput ["Calling: ", minimap2Path, unwords cmdargs]
    let cp = proc minimap2Path cmdargs
    usam <- makeNGLTempFile "fastq" "minimap2." "sam" $ \hout -> do
        (exitCode, _, err) <- with1Thread $
                CP.sourceProcessWithStreams cp
                    (interleaveFQs rs) -- stdin
                    (CB.sinkHandle hout) -- stdout
                    CL.consume -- stderr
        let err' = BL8.unpack $ BL8.fromChunks err
        outputListLno' DebugOutput ["minimap2 info: ", err']
        case exitCode of
            ExitSuccess ->
                outputListLno' InfoOutput ["Finished mapping to ", refIndex]
            ExitFailure code ->
                throwSystemError $ concat ["Failed mapping\n",
                                "Executable used::\t", minimap2Path,"\n",
                                "Command line was::\n\t", unwords cmdargs, "\n",
                                "minimap2 error code was ", show code, ".\n",
                                "minimap2 stderr: ", err']
    C.runConduit $ sortSam usam
                        .| CL.map (`B.snoc` 10)
                        .| outC

sortSam :: FilePath -> C.ConduitT () B.ByteString NGLessIO ()
sortSam samfile =
        CB.sourceFile samfile
            .| linesC
            .| CL.map unwrapByteLine
            .| do
                CC.takeWhile isSamHeaderString
                partials <- samSorter []
                C.toProducer (mergeC partials)
    where
        samSorter :: [C.ConduitT () B.ByteString NGLessIO () ] -> C.ConduitM B.ByteString B.ByteString NGLessIO [C.ConduitT () B.ByteString NGLessIO ()]
        samSorter partials = do
                block <- CC.sinkVectorN (1024*1024)
                block' <- liftIO $ do
                    numCapabilities <- getNumCapabilities
                    block' <- V.unsafeThaw block
                    sortParallel numCapabilities block'
                    V.unsafeFreeze block'
                isDone <- CC.null
                if isDone
                    then return (CC.yieldMany block':partials)
                    else do
                        partial <- lift $
                            makeNGLTempFile samfile "partial" "sam" $ \hout ->
                                C.runConduit $
                                    CC.yieldMany block'
                                        .| CL.map (`B.snoc` 10)
                                        .| CB.sinkHandle hout
                        samSorter ((CB.sourceFile partial .| linesC .| CL.map unwrapByteLine):partials)

