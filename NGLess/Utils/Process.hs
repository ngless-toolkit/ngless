module Utils.Process
    ( runProcess
    ) where
import           System.Exit (ExitCode(..))
import           System.Process (proc)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import qualified UnliftIO as U
import           Control.Concurrent (getNumCapabilities, setNumCapabilities)

import Output
import NGLess
import Configuration
import NGLess.NGLEnvironment

-- | runProcess and check exit code
runProcess :: FilePath -- ^ executable
                -> [String] -- ^ command line arguments
                -> C.ConduitT () B.ByteString NGLessIO () -- ^ stdin
                -> C.ConduitT B.ByteString C.Void NGLessIO a -- ^ stdout
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
    outputListLno' DebugOutput ["Will run process ", binPath, unwords args]
    (exitCode, out, err) <- with1Thread $
        CP.sourceProcessWithStreams
            (proc binPath args)
            stdin
            stdout
            CL.consume
    let err' = BL8.unpack $ BL8.fromChunks err
    outputListLno' DebugOutput ["Stderr: ", err']
    case exitCode of
        ExitSuccess -> do
            outputListLno' InfoOutput ["Success"]
            return out
        ExitFailure code ->
            throwSystemError $ concat ["Failed command\n",
                            "Executable used::\t", binPath,"\n",
                            "Command line was::\n\t", unwords args, "\n",
                            "Error code was ", show code, ".\n",
                            "Stderr: ", err']
