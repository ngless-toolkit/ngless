{-# LANGUAGE RecordWildCards #-}
{- Copyright 2015 NGLess Authors
 - License: MIT
 -}

module Utils.LockFile
    ( withLockFile
    , LockParameters(..)
    , WhenExistsStrategy(..)
    ) where

import System.Posix.Process
import System.IO.Error (isDoesNotExistError)
import System.Directory
import Data.Time
import Control.Monad.Except
import Control.Exception
import Control.Concurrent (threadDelay)

import Data.Bits ((.|.))
import Foreign.C (eEXIST, errnoToIOError, getErrno)
import GHC.IO.Handle.FD (fdToHandle)
import System.IO (Handle, hClose, hPutStrLn)
import System.Posix.Internals
    ( c_close
    , c_open
    , o_BINARY
    , o_CREAT
    , o_EXCL
    , o_NOCTTY
    , o_NONBLOCK
    , o_RDWR
    , withFilePath
    )
import Control.Monad.Trans.Resource
import Network.BSD (getHostName)

import NGLess
import Output
import FileManagement
import Utils.Utils (maybeM)


data LockParameters = LockParameters
                { lockFname :: FilePath
                , maxAge :: NominalDiffTime
                , whenExistsStrategy :: WhenExistsStrategy
                } deriving (Eq, Show)

data WhenExistsStrategy = IfLockedThrow NGError | IfLockedRetry { nrLockRetries :: !Int, timeBetweenRetries :: !NominalDiffTime }
            deriving (Eq, Show)

withLockFile :: LockParameters -> NGLessIO a -> NGLessIO a
withLockFile params act =
    acquireLock params >>= \case
        Just rk -> do
            outputListLno' DebugOutput ["Acquired lock file ", lockFname params]
            v <- act
            release rk
            return v
        Nothing -> do
            outputListLno' InfoOutput ["Lock file exists ", lockFname params]
            lockExists params act


lockExists params@LockParameters{..} act = liftIO (fileAge lockFname) >>= \case
        Nothing -> do
            outputListLno' InfoOutput ["Lock file ", lockFname, " existed but has been removed. Retrying."]
            withLockFile params act
        Just age | age > maxAge -> do
            outputListLno' InfoOutput ["Lock file ", lockFname, " exists but is too old. Assuming it is stale and removing it."]
            liftIO $ removeFileIfExists lockFname
            withLockFile params act
        _ -> case whenExistsStrategy of
            IfLockedThrow err -> throwError err
            IfLockedRetry{ .. }
                | nrLockRetries > 0 -> do
                        outputListLno' InfoOutput ["Lock file ", lockFname, " exists and seems current, sleeping for ", show timeBetweenRetries, "."]
                        liftIO $ sleep timeBetweenRetries
                        let lessOneTry = IfLockedRetry (nrLockRetries - 1) timeBetweenRetries
                        lockExists params { whenExistsStrategy = lessOneTry } act
                | otherwise -> throwSystemError ("Could not obtain lock " ++ lockFname ++ " even after waiting for its release.")

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds
    where
        toMicroSeconds :: NominalDiffTime -> Int
        toMicroSeconds = (1000000 *) . fromInteger . round

acquireLock :: LockParameters -> NGLessIO (Maybe ReleaseKey)
acquireLock LockParameters{..} = liftIO (openLockFile lockFname) `maybeM` \h -> do
    -- rkC is for the case where an exception is raised between this line and the release call below
    rkC <- register (hClose h)
    rk <- register (removeFileIfExists lockFname)
    outputListLno' DebugOutput ["Acquired lock file ", lockFname]
    liftIO $ do
        pid <- getProcessID
        hostname <- getHostName
        hPutStrLn h ("Lock file created for PID " ++ show pid ++ " on hostname " ++ hostname)
        release rkC
        return (Just rk)

-- This code is adapted from https://hackage.haskell.org/package/lock-file-0.5.0.2/docs/src/System-IO-LockFile-Internal.html
openLockFile :: FilePath -> IO (Maybe Handle)
openLockFile lockFileName = do
    let openFlags = o_NONBLOCK .|. o_NOCTTY .|. o_RDWR .|. o_CREAT .|. o_EXCL .|. o_BINARY
    fd <- withFilePath lockFileName $ \ fp -> c_open fp openFlags 0o644
    if fd > 0
        then Just <$> fdToHandle fd `onException` c_close fd
        else do
            errno <- getErrno
            when (errno /= eEXIST) . ioError
                . errnoToIOError "lock" errno Nothing $ Just lockFileName
            -- Failed to open lock file because it already exists
            return Nothing

handleIf cond alt act = handleJust
    (\e -> if cond e then return (Just ()) else return Nothing)
    (const alt)
    act

fileAge :: FilePath -> IO (Maybe NominalDiffTime)
fileAge fname = handleIf isDoesNotExistError (return Nothing) $ Just <$> do
    mtime <- getModificationTime fname
    cur <- getCurrentTime
    return (cur `diffUTCTime` mtime)

