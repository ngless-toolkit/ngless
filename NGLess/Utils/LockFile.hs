{-# LANGUAGE RecordWildCards, CPP #-}
{- Copyright 2015-2019 NGLess Authors
 - License: MIT
 -}

module Utils.LockFile
    ( withLockFile
    , LockParameters(..)
    , WhenExistsStrategy(..)
    , acquireLock
    , fileAge
    , removeFileIfExists
    ) where

import qualified Control.Concurrent.Async as A

#ifndef WINDOWS
import           System.Posix.Process
import           System.Posix.Files (touchFile)
#endif


import System.IO.Error (isDoesNotExistError)
import System.Directory (getModificationTime, removeFile)
import Data.Time (NominalDiffTime
                 , getZonedTime
                 , formatTime
                 , defaultTimeLocale
                 , getCurrentTime
                 , diffUTCTime
                 )
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
import Network.HostName (getHostName)

import NGLess.NGError
import Output

data LockParameters = LockParameters
                { lockFname :: FilePath
                , maxAge :: NominalDiffTime
                , whenExistsStrategy :: WhenExistsStrategy
                , mtimeUpdate :: Bool -- ^ start a thread which updates the mtime on the file every 10 minutes
                } deriving (Eq, Show)

data WhenExistsStrategy =
                IfLockedNothing
                | IfLockedThrow NGError
                | IfLockedRetry { nrLockRetries :: !Int, timeBetweenRetries :: !NominalDiffTime }
            deriving (Eq, Show)

pidAsStr :: IO String
#ifndef WINDOWS
pidAsStr = show <$> getProcessID
#else
pidAsStr = return "(PID is not available on Windows)"
#endif

#ifdef WINDOWS
touchFile fname = writeFile fname "lock file"
#endif

-- | Executes the action specified with a lock file around it so that multiple
-- ngless do not clash with each other
withLockFile :: LockParameters -> NGLessIO a -> NGLessIO a
withLockFile params act =
    acquireLock params >>= \case
        Just rk -> do
            v <- act
            release rk
            return v
        Nothing -> throwSystemError "Could not acquire required lock file."


sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds
    where
        toMicroSeconds :: NominalDiffTime -> Int
        toMicroSeconds = (1000000 *) . fromInteger . round


-- | Atomically create a lock file
-- If file already exists, returns 'Nothing'
acquireLock :: LockParameters -> NGLessIO (Maybe ReleaseKey)
acquireLock params@LockParameters{..} = liftIO (openLockFile lockFname) >>= \case
    Just h -> do
        -- rkC is for the case where an exception is raised between this line and the release call below
        rkC <- register (hClose h)
        rk <- register (removeFileIfExists lockFname)
        outputListLno' DebugOutput ["Acquired lock file ", lockFname]
        liftIO $ do
            pid <- pidAsStr
            hostname <- getHostName
            t <- getZonedTime
            let tformat = "%a %d-%m-%Y %R"
                tstr = formatTime defaultTimeLocale tformat t
            hPutStrLn h ("Lock file created for PID " ++ pid ++ " on hostname " ++ hostname ++ " at time " ++ tstr)
            release rkC
        Just <$> if mtimeUpdate
                then do
                    let updateloop :: IO ()
                        updateloop = threadDelay (10*60*1000*1000) >> touchFile lockFname >> updateloop
                    (rk', _) <- allocate (A.async updateloop) A.cancel
                    register (release rk' >> release rk)
                else return rk
    Nothing -> liftIO (fileAge lockFname) >>= \case
        Nothing -> do
            outputListLno' InfoOutput ["Lock file ", lockFname, " existed but has been removed. Retrying."]
            acquireLock params
        Just age | age > maxAge -> do
            outputListLno' InfoOutput ["Lock file ", lockFname, " exists but is too old. Assuming it is stale and removing it."]
            liftIO $ removeFileIfExists lockFname
            acquireLock params
        _ -> case whenExistsStrategy of
            IfLockedNothing -> return Nothing
            IfLockedThrow err -> throwError err
            IfLockedRetry{ .. }
                | nrLockRetries > 0 -> do
                    outputListLno' InfoOutput ["Lock file ", lockFname, " exists and seems current, sleeping for ", show timeBetweenRetries, "."]
                    liftIO $ sleep timeBetweenRetries
                    let lessOneTry = IfLockedRetry (nrLockRetries - 1) timeBetweenRetries
                    acquireLock (params { whenExistsStrategy = lessOneTry })
                | otherwise -> throwSystemError ("Could not obtain lock " ++ lockFname ++ " even after waiting for its release.")

-- This code is adapted from
-- https://hackage.haskell.org/package/lock-file-0.5.0.2/docs/src/System-IO-LockFile-Internal.html
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

handleIf :: Exception e => (e -> Bool) -> IO a -> IO a -> IO a
handleIf cond alt act = handleJust
    (\e -> if cond e then return (Just ()) else return Nothing)
    (const alt)
    act

fileAge :: FilePath -> IO (Maybe NominalDiffTime)
fileAge fname = handleIf isDoesNotExistError (return Nothing) $ Just <$> do
    mtime <- getModificationTime fname
    cur <- getCurrentTime
    return (cur `diffUTCTime` mtime)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = removeFile fp `catch` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e
