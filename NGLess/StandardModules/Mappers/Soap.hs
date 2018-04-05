{- Copyright 2016-2017 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module StandardModules.Mappers.Soap
    ( hasValidIndex
    , createIndex
    , callMapper
    ) where

import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.TQueue as CA
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit (($$), (.|))
import           Control.Monad.Extra (guard, allM, whenM)
import           GHC.Conc (getNumCapabilities, setNumCapabilities)
import           Data.List (isSuffixOf)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Trans.Resource
import           Control.Exception (bracket)

import Output
import NGLess
import Utils.Conduit
import FileManagement
import Configuration
import NGLess.NGLEnvironment
import Utils.Utils (dropEnd)

hasValidIndex :: FilePath -> NGLessIO Bool
hasValidIndex basepath = liftIO $ flip allM indexRequiredFormats $ \ext -> doesFileExist (basepath' ++ ext)
    where
        basepath'
            | ".gz" `isSuffixOf` basepath = dropEnd 3 basepath
            | otherwise = basepath
        indexRequiredFormats =
                [".index.amb"
                ,".index.ann"
                ,".index.bwt"
                ,".index.fmv"
                ,".index.hot"
                ,".index.lkt"
                ,".index.pac"
                ,".index.rev.bwt"
                ,".index.rev.fmv"
                ,".index.rev.lkt"
                ,".index.rev.pac"
                ,".index.sa"
                ,".index.sai"
                ]


createIndex :: FilePath -> NGLessIO ()
createIndex fafile = do
    outputListLno' InfoOutput ["Start SOAP index creation for ", fafile]
    fafile' <- if ".gz" `isSuffixOf` fafile
                    then do
                        let gunzipped = dropEnd 3 fafile
                        outputListLno' WarningOutput ["SOAP indexing does not work on gzipped files directly. Gunzipping '", fafile, "'..."]
                        whenM (liftIO $ doesFileExist gunzipped) $
                            throwDataError ("SOAP indexing does not work on gzipped files (got argument '" ++ fafile ++ "' and gunzipped version already exists (so refusing to overwrite).")
                        conduitPossiblyCompressedFile fafile $$ CB.sinkFile gunzipped
                        return gunzipped
                    else return fafile
    (exitCode, out, err) <- liftIO $
        readProcessWithExitCode "2bwt-builder" [fafile'] []
    outputListLno' DebugOutput ["SOAP-index stderr: ", err]
    outputListLno' DebugOutput ["SOAP-index stdout: ", out]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _err -> throwSystemError err

callMapper :: FilePath -> [FilePath] -> [String] -> C.Consumer B.ByteString IO a -> NGLessIO a
callMapper refIndex fps extraArgs outC = do
    outputListLno' InfoOutput ["Starting mapping to ", refIndex]
    numCapabilities <- liftIO getNumCapabilities
    strictThreads <- nConfStrictThreads <$> nglConfiguration
    (rk,(otemp,htemp)) <- openNGLTempFile' refIndex "map_" "soap"
    (rk2,(otemp2,htemp2)) <- openNGLTempFile' refIndex "map2_" "soap"
    liftIO $ hClose htemp
    liftIO $ hClose htemp2
    let soapThreads
            | strictThreads && numCapabilities > 1 = numCapabilities - 1
            | otherwise = numCapabilities
        fps' = case fps of
                        [fp] -> ["-a", fp]
                        [fp, fp'] -> ["-a", fp, "-b", fp', "-2", otemp2]
                        _ -> error "SOAP Multiple errors"
        cmdargs =  concat [fps', ["-p", show soapThreads, "-D", refIndex ++ ".index", "-o", otemp], extraArgs]
        soapPath = "soap2.21"
        with1Thread act
            | strictThreads = bracket
                                (setNumCapabilities 1)
                                (const $ setNumCapabilities numCapabilities)
                                (const act)
            | otherwise = act
    outputListLno' TraceOutput ["Calling binary ", soapPath, " with args: ", unwords cmdargs]
    let cp = proc soapPath cmdargs
    (exitCode, (), err) <- liftIO $ with1Thread $
            CP.sourceProcessWithStreams cp
                (return ()) -- stdin
                (return ()) -- stdout
                CL.consume -- stderr
    outputListLno' DebugOutput ["SOAP info: ", BL8.unpack $ BL8.fromChunks err]
    case exitCode of
        ExitFailure code ->
            throwSystemError $ concat ["Failed mapping\nCommand line was::\n\t",
                            soapPath, " ", unwords cmdargs, "\n",
                            "SOAP error code was ", show code, "."]
        ExitSuccess -> return ()
    outputListLno' InfoOutput ["Done mapping to ", refIndex, ". Converting to SAM..."]
    let cmdargs' =  ["-p" | length fps == 2]
        soap2samPath = "soap2sam.pl"
        soup2sam = proc soap2samPath cmdargs'
    -- We are going to create a thread which simply consumes input and feeds it
    -- to `outC`. Then we feed it both the SAM header and the result of
    -- soup2sam.  This is possibly not the simplest way to achieve this goal,
    -- but without having a resumable sink, I do not see exactly how to get it
    -- all to work.
    q <- liftIO $ TQ.newTBMQueueIO 4
    out <- liftIO $ A.async (CA.sourceTBMQueue q $$ outC)
    C.runConduitRes $
        makeSAMHeader refIndex
            .| CA.sinkTBMQueue q False
    (exitCode', (), err') <- liftIO $
        withFile otemp ReadMode $ \otemph ->
            withFile otemp2 ReadMode $ \otemp2h ->
                CP.sourceProcessWithStreams soup2sam
                    (CB.sourceHandle otemph >> CB.sourceHandle otemp2h) -- stdin
                    (C.toConsumer $ CA.sinkTBMQueue q True) -- stdout
                    (CL.consume :: C.Consumer B.ByteString IO [B.ByteString])
    release rk
    release rk2
    case exitCode' of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Done soap2sam."]
            liftIO $ A.wait out
        ExitFailure code ->
            throwSystemError $ concat ["Failed conversion to SAM\nCommand line was::\n\t",
                            soap2samPath, " ", unwords cmdargs', "\n",
                            "SOAP2SAM error code was ", show code, ".\n",
                            "Error output: ", B8.unpack (B8.intercalate "\n\t" err')]

makeSAMHeader :: (MonadResource m, MonadBaseControl IO m, MonadError NGError m) => FilePath -> C.Source m B.ByteString
makeSAMHeader fafile = conduitPossiblyCompressedFile fafile .| linesC .| asSamHeader
    where
        -- asSamHeader :: C.Conduit ByteLine IO B.ByteString
        asSamHeader = awaitJust $ \(ByteLine line) -> case readId line of
                                                Just rid -> processRead rid 0
                                                Nothing -> return ()
        -- processRead :: B.ByteString -> Int -> C.Conduit ByteLine IO B.ByteString
        processRead rid !n = C.await >>= \case
            Nothing -> emit rid n
            Just (ByteLine line) -> case readId line of
                Just rid' -> do
                    emit rid n
                    processRead rid' 0
                Nothing -> processRead rid (n + B.length line)
        readId :: B.ByteString -> Maybe B.ByteString
        readId line = do
            (mark,rest) <- B8.uncons line
            guard (mark == '>')
            return $ head (B8.split ' ' rest)
        emit rid n = C.yield $ B.concat ["@SQ\tSN:", rid, "\t", B8.pack (show n), "\n"]
