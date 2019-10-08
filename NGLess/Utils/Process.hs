module Utils.Process
    ( runProcess
    ) where
import           System.Exit (ExitCode(..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Conduit.Process.Typed as CPT
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit ((.|))
import qualified UnliftIO as U
import           Control.Concurrent (getNumCapabilities, setNumCapabilities)
import qualified Control.Monad.STM as STM

import Output
import NGLess
import Configuration
import NGLess.NGLEnvironment

-- | runProcess and check exit code
runProcess :: FilePath -- ^ executable
                -> [String] -- ^ command line arguments
                -> C.ConduitT () B.ByteString NGLessIO () -- ^ stdin
                -> Either a (C.ConduitT B.ByteString C.Void NGLessIO a) -- ^ stdout: 'Right sink' if it's a consumer, else always return the value given
                -> NGLessIO a
runProcess binPath args stdin stdout = do
    numCapabilities <- liftIO getNumCapabilities
    strictThreads <- nConfStrictThreads <$> nglConfiguration
    let with1Thread act
            | strictThreads = U.bracket_
                                (liftIO $ setNumCapabilities 1)
                                (liftIO $ setNumCapabilities numCapabilities)
                                act
            | otherwise = act
        stdout' = case stdout of
            Left _ -> fmap Left CL.consume
            Right sink -> fmap Right sink
    outputListLno' DebugOutput ["Will run process ", binPath, unwords args]
    (exitCode, out, err) <- with1Thread $
        CPT.withProcessWait (
                        -- No need to keep these open
                        CPT.setCloseFds True
                        -- We need the Handle for stdin because we need to hClose it!
                        -- Therefore, we cannot use `CPT.setStdin CTP.createSink`
                        $ CPT.setStdin CPT.createPipe
                        $ CPT.setStderr CPT.byteStringOutput
                        $ CPT.setStdout CPT.createSource
                        $ CPT.proc binPath args) $ \p ->
            U.runConcurrently $ (,,)
                <$> U.Concurrently (CPT.waitExitCode p)
                <*  U.Concurrently (do
                                        let hin = CPT.getStdin p
                                        C.runConduit (stdin .| CC.sinkHandle hin)
                                        U.hClose hin)
                <*> U.Concurrently (C.runConduit (CPT.getStdout p .| stdout'))
                <*> U.Concurrently (liftIO $ STM.atomically (CPT.getStderr p))
    let err' = BL8.unpack err
    outputListLno' DebugOutput ["Stderr: ", err']
    (r, out') <- case out of
        Left str -> do
            outputListLno' DebugOutput ["Stdout: ", BL8.unpack $ BL8.fromChunks str]
            return $! case stdout of
                            Left f -> (f, BL8.unpack $ BL8.fromChunks str)
                            Right _ -> error "absurd"
        Right v -> return (v, "<captured by inner process>\n")
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Success"]
            return r
        ExitFailure code ->
            throwSystemError $ concat ["Failed command\n",
                            "Executable used::\t", binPath,"\n",
                            "Command line was::\n\t", unwords args, "\n",
                            "Error code was ", show code, ".\n",
                            "Stdout: ", out',
                            "Stderr: ", err']
