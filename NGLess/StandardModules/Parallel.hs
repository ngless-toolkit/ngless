{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, CPP #-}

module StandardModules.Parallel
    ( loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Time (getZonedTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.List.Extra (snoc, chunksOf)

#ifndef WINDOWS
import           System.Posix.Unistd (fileSynchronise)
import           System.Posix.IO (openFd, defaultFileFlags, closeFd, OpenMode(..), handleToFd)
import           System.Posix.Files (touchFile)
#endif


import           System.FilePath
import           GHC.Conc (getNumCapabilities, atomically)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TQueue as CA
import           Control.Monad.ST
import           Control.DeepSeq
import           Data.Traversable
import           Control.Concurrent (threadDelay)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import System.IO
import Data.Default
import Control.Monad
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile)

import qualified Data.Hash.MD5 as MD5
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((=$=), ($$))

import Hooks
import Output
import NGLess
import Modules
import Language
import Transform
import FileOrStream
import Configuration
import FileManagement

import Utils.Utils
import Utils.Conduit
import Utils.LockFile

syncFile :: FilePath -> IO ()
#ifndef WINDOWS
syncFile fname = do
    withFile fname ReadWriteMode $ handleToFd >=> fileSynchronise
    -- withFile does not support directories
    -- The code below will also not work on Windows
    fd <- openFd (takeDirectory fname) ReadOnly Nothing defaultFileFlags
    fileSynchronise fd
    closeFd fd

#else
syncFile _ = return ()
#endif

#ifdef WINDOWS
touchFile fname = writeFile fname "lock file"
#endif

setupHashDirectory :: FilePath -> T.Text -> NGLessIO FilePath
setupHashDirectory basename hash = do
    let actiondir = basename </> take 8 (T.unpack hash)
    liftIO $ createDirectoryIfMissing True actiondir
    return actiondir

executeLock1 (NGOList entries) kwargs  = do
    entries' <- mapM (stringOrTypeError "lock1") entries
    hash <- lookupStringOrScriptError "lock1" "__hash" kwargs
    lockdir <- setupHashDirectory "ngless-locks" hash
    (e,rk) <- getLock lockdir entries'
    registerHook FinishOkHook $ do
        let receiptfile = lockdir </> T.unpack e ++ ".finished"
        liftIO $ withFile receiptfile WriteMode $ \h -> do
            t <- getZonedTime
            let tformat = "%a %d-%m-%Y %R"
                tstr = formatTime defaultTimeLocale tformat t
            hPutStrLn h (concat ["Finished ", T.unpack e, " at ", tstr])
        release rk
    return $! NGOString e

executeLock1 arg _ = throwScriptError ("Wrong argument for lock1 (expected a list of strings, got `" ++ show arg ++ "`")

getLock _ [] = do
   outputListLno' InfoOutput ["Could get a lock for any file."]
   throwGenericError "Could not obtain any lock"
getLock basedir (x:xs) = do
    let fname = T.unpack x
        lockname = basedir </> fname ++ ".lock"
    finished <- liftIO $ doesFileExist (basedir </> fname ++ ".finished")
    if finished
        then getLock basedir xs
        else acquireLock' (LockParameters
                            { lockFname = lockname
                            , maxAge = fromInteger (60*60)
                                -- one hour. Given that lock files are touched
                                -- every ten minutes if things are good (see
                                -- thread below), this is an indication that
                                -- the process has crashed
                            , whenExistsStrategy = IfLockedNothing}) >>= \case
            Nothing -> getLock basedir xs
            Just rk -> do
                let updateloop :: IO ()
                    updateloop = threadDelay (10*60*1000*1000) >> touchFile lockname >> updateloop
                void . liftIO . A.async $ updateloop
                return (x,rk)

executeCollect :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeCollect (NGOCounts istream) kwargs = do
    current <- lookupStringOrScriptError "collect arguments" "current" kwargs
    allentries <- lookupStringListOrScriptError "collect arguments" "allneeded" kwargs
    ofile <- lookupStringOrScriptError "collect arguments" "ofile" kwargs
    canMove <- lookupBoolOrScriptErrorDef (return False) "collect hidden argument" "__can_move" kwargs
    hash <- lookupStringOrScriptError "lock1" "__hash" kwargs
    hashdir <- setupHashDirectory "ngless-partials" hash
    countfile <- asFile istream
    let partialfile entry = hashdir </> T.unpack entry <.> "part"
    liftIO $ syncFile countfile
    liftIO $ (if canMove then moveOrCopy else copyFile) countfile (partialfile current)
    canCollect <- and <$> forM allentries (liftIO . doesFileExist . partialfile)
    when canCollect $ do
        newfp <- concatCounts allentries (map partialfile allentries)
        liftIO $ moveOrCopy newfp (T.unpack ofile)
    return NGOVoid
executeCollect arg _ = throwScriptError ("collect got unexpected argument: " ++ show arg)


-- | split a list into a given number of (roughly) equally sized chunks
nChunks :: Int -- ^ number of chunks
            -> [a] -> [[a]]
nChunks 1 xs = [xs]
nChunks n xs = chunksOf p xs
    where
        p = 1 + (length xs `div` n)

splitLines :: [V.Vector B.ByteString] -> NGLess (V.Vector B.ByteString, V.Vector B.ByteString)
splitLines = mapM splitLine1 >=> groupLines
    where
        splitLine1 :: V.Vector B.ByteString -> NGLess (V.Vector B.ByteString, V.Vector B.ByteString)
        splitLine1 ells = runST $ do
            let n = V.length ells
            headers <- VM.new n
            contents <- VM.new n
            let split1 False _ _ = return False
                split1 _ ix line = case B8.elemIndex '\t' line of
                            Nothing -> return False
                            Just p -> do
                                let (h, c) = B.splitAt p line
                                VM.write headers ix h
                                VM.write contents ix c
                                return True
            isOk <- V.ifoldM split1 True ells
            if isOk
               then do
                   headers' <- V.unsafeFreeze headers
                   contents' <- V.unsafeFreeze contents
                   return . Right $ (headers', contents')
               else return $ throwDataError "Line does not have a TAB character"

groupLines :: [(V.Vector B.ByteString, V.Vector B.ByteString)] -> NGLess (V.Vector B.ByteString, V.Vector B.ByteString)
groupLines [] = return (V.empty, V.empty)
groupLines groups
    | allSame (fst <$> groups) = return (fst . head $ groups, V.unfoldr catContent 0)
    | otherwise = throwDataError "Headers do not match"
    where
        n = V.length (head contents)
        contents = snd <$> groups
        catContent ix
            | ix == n = Nothing
            | otherwise = Just (B.concat (map (V.! ix) contents), ix + 1)

concatPartials :: [(V.Vector B.ByteString, V.Vector B.ByteString)] -> NGLess BL.ByteString
concatPartials [] = throwShouldNotOccur "concatPartials of empty set"
concatPartials input = do
    (header, contents) <- groupLines input
    return . BL.fromChunks $ concatMap (\ix -> [header V.! ix, contents V.! ix, "\n"]) [0 .. V.length header - 1]


-- | strict variation of sinkTBMQueue
sinkTBMQueue' q shouldClose = do
        C.awaitForever $ \ !v -> liftSTM (TQ.writeTBMQueue q v)
        when shouldClose (liftSTM $ TQ.closeTBMQueue q)
    where
        liftSTM = liftIO . atomically

-- If the number of input files is very large (>1024, typically), we risk
-- hitting the limit on open files by a process, so we work in batches of 512.
maxNrOpenFiles = 512 :: Int

concatCounts :: [T.Text] -> [FilePath] -> NGLessIO FilePath
concatCounts headers inputs
    | length inputs > maxNrOpenFiles = do
        let current = take maxNrOpenFiles inputs
            currenth = take maxNrOpenFiles headers
            rest = drop maxNrOpenFiles inputs
            resth = drop maxNrOpenFiles headers
        first <- concatCounts currenth current
        concatCounts (snoc resth $ T.intercalate "\t" currenth) (snoc rest first)
    | otherwise = do
        (newfp,hout) <- openNGLTempFile "collected" "collected.counts." "txt"
        liftIO $ T.hPutStrLn hout (T.intercalate "\t" ("":headers))
        numCapabilities <- liftIO getNumCapabilities
        let mapthreads = max 1 (numCapabilities - 1)
            sources =
                [conduitPossiblyCompressedFile f
                    =$= CB.lines
                    =$= (C.await >> C.awaitForever C.yield) -- drop header line
                    =$=  (C.conduitVector 2048 :: C.Conduit B.ByteString (ResourceT IO) (V.Vector B.ByteString))
                    | f <- inputs]
            sourcesplits = nChunks mapthreads sources
        channels <- liftIO $ forM sourcesplits $ \ss -> do
            ch <- TQ.newTBMQueueIO 4
            a <- A.async $ runResourceT (C.sequenceSources ss =$= CL.map (force . splitLines) $$ sinkTBMQueue' ch True)
            return (CA.sourceTBMQueue ch, a)
        C.sequenceSources (fst <$> channels)
            =$= asyncMapEitherC mapthreads (sequence >=> concatPartials)
            $$ C.sinkHandle hout
        forM_ (snd <$> channels) (liftIO . A.wait)
        liftIO (hClose hout)
        return newfp


lock1 = Function
    { funcName = FuncName "lock1"
    , funcArgType = Just (NGList NGLString)
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    }

collectFunction = Function
    { funcName = FuncName "collect"
    , funcArgType = Just NGLCounts
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs =
        [ArgInformation "current" True NGLString []
        ,ArgInformation "allneeded" True (NGList NGLString) []
        ,ArgInformation "ofile" True NGLString []
        ]
    , funcAllowsAutoComprehension = False
    }

addHash :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
addHash script = do
        isSubsample <- nConfSubsample <$> nglConfiguration
        pureTransform (addHash' isSubsample) script
    where
        addHash' :: Bool -> Expression -> Expression
        addHash' isSubsample (FunctionCall (FuncName fn) expr kwargs block)
            | fn `elem` ["lock1", "collect"] = FunctionCall (FuncName fn) expr ((Variable "__hash", ConstStr h):kwargs) block
            where
                h = T.pack . (++ (if isSubsample then "-subsample" else "")) . MD5.md5s . MD5.Str . show $
                        case fn of
                            "lock1" -> map snd script
                            "collect" -> expr:map snd script
                            _ -> error "impossible case"
        addHash' _ e = e

loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.parallel" "0.0"
        , modFunctions = [lock1, collectFunction]
        , modTransform = addHash
        , runFunction = \case
            "lock1" -> executeLock1
            "collect" -> executeCollect
            _ -> error "Bad function name"
        }

