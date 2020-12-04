{- Copyright 2016-2020 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE FlexibleContexts, CPP #-}

module StandardModules.Parallel
    ( loadModule
    , pasteCounts
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Time (getZonedTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.List.Extra (snoc, chunksOf)

#ifndef WINDOWS
import           System.Posix.Unistd (fileSynchronise)
import           System.Posix.IO (openFd, defaultFileFlags, closeFd, OpenMode(..))
import           Control.Exception (bracket)
#endif


import           System.FilePath
import           GHC.Conc (getNumCapabilities, atomically)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TQueue as CA
import qualified Data.Conduit.Algorithms as CAlg
import qualified Data.Conduit.Algorithms.Async as CAlg
import           Data.Conduit.Algorithms.Async (conduitPossiblyCompressedFile)
import           Control.Monad.ST (runST)
import           Control.Monad.Except (throwError)
import           Control.Monad.Extra (allM, unlessM)
import           Control.DeepSeq
import           Data.Traversable
import           Control.Monad.Trans.Class
import           System.AtomicWrite.Writer.Text (atomicWriteFile)


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.State.Lazy
import System.IO
import Data.IORef
import Data.Default
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)

import qualified Data.Hash.MD5 as MD5
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((.|), (.|), ($$+))

import Hooks
import Output
import NGLess
import Modules
import Language
import Transform
import FileOrStream
import Configuration
import FileManagement
import NGLess.NGError
import NGLess.NGLEnvironment

import Interpretation.Write (moveOrCopyCompress)

import Utils.Utils (fmapMaybeM, allSame, moveOrCopy)
import Utils.Conduit
import Utils.LockFile

prefixRef :: IORef String
{-# NOINLINE prefixRef #-}
prefixRef = unsafePerformIO (newIORef "")

syncFile :: FilePath -> IO ()
#ifndef WINDOWS
syncFile fname = do
    bracket (openFd fname ReadWrite Nothing defaultFileFlags)
        closeFd
        fileSynchronise
    -- The code below will not work on Windows
    bracket (openFd (takeDirectory fname) ReadOnly Nothing defaultFileFlags)
        closeFd
        fileSynchronise

#else
syncFile _ = return ()
#endif


setupHashDirectory :: FilePath -> T.Text -> NGLessIO FilePath
setupHashDirectory basename hash = do
    prefix <- liftIO $ readIORef prefixRef
    isSubsample <- nConfSubsample <$> nglConfiguration
    let actiondir = basename </> prefix ++ take 8 (T.unpack hash) ++ (if isSubsample then "-subsample" else "")
        scriptfile = actiondir </> "script.ngl"
    liftIO $ createDirectoryIfMissing True actiondir
    unlessM (liftIO $ doesFileExist scriptfile) $ do
        sct <- ngleScriptText <$> nglEnvironment
        liftIO $ atomicWriteFile scriptfile sct
    return actiondir

-- Beware that addition of characters here can lead to lock collisions
-- as is "project/sample" clashes with "project_sample" but ... uncommon case
unsafeCharMap = [('/', '_'),
                 ('\\', '_')]

-- | Remove '/' and '\' from filenames
sanitizePath :: T.Text -> T.Text
sanitizePath = T.map (\x -> fromMaybe x (lookup x unsafeCharMap))

executeLock1 (NGOList entries) kwargs  = do
    entries' <- mapM (stringOrTypeError "lock1") entries
    hash <- lookupStringOrScriptError "lock1" "__hash" kwargs
    lockdir <- setupHashDirectory "ngless-locks" hash
    -- Keep a map of 'sane -> original' names used for locks to backtrace
    -- what file was locked and return the unsanitized name
    -- See also https://github.com/ngless-toolkit/ngless/issues/68
    let saneentries = sanitizePath <$> entries'
        lockmap = zip saneentries entries'
    (e,rk) <- getLock lockdir saneentries
    outputListLno' InfoOutput ["lock1: Obtained lock file: '", lockdir </> T.unpack e ++ ".lock", "'"]
    reportbase <- setupHashDirectory "ngless-stats" hash
    let reportdir = reportbase </> T.unpack e
    outputListLno' InfoOutput ["Writing stats to '", reportdir, "'"]
    let setReportDir c = c { nConfReportDirectory = reportdir }
    updateNglEnvironment $ \env -> env { ngleConfiguration = setReportDir (ngleConfiguration env) }
    registerHook FinishOkHook $ do
        let receiptfile = lockdir </> T.unpack e ++ ".finished"
        liftIO $ withFile receiptfile WriteMode $ \h -> do
            t <- getZonedTime
            let tformat = "%a %d-%m-%Y %R"
                tstr = formatTime defaultTimeLocale tformat t
            hPutStrLn h (concat ["Finished ", T.unpack e, " at ", tstr])
        release rk
    registerFailHook $ do
        let logfile = lockdir </> T.unpack e ++ ".failed"
        withFile logfile WriteMode $ \h ->
            hPutStrLn h "Execution failed" -- TODO output log here
    return $! NGOString $ fromMaybe e $ lookup e lockmap

executeLock1 arg _ = throwScriptError ("Wrong argument for lock1 (expected a list of strings, got `" ++ show arg ++ "`")


lockName = (++ ".lock") . T.unpack
finishedName = (++ ".finished") . T.unpack
failedName = (++ ".failed") . T.unpack

-- | Create a lock file
getLock :: FilePath
                -- ^  directory where to create locks
                -> [T.Text]
                -- ^ keys to attempt to ock
                -> NGLessIO (T.Text, ReleaseKey)
getLock basedir fs = do
    existing <- liftIO $ getDirectoryContents basedir
    let notfinished = flip filter fs $ \fname -> finishedName fname `notElem` existing
        notlocked = flip filter notfinished $ \fname -> lockName fname `notElem` existing
        notfailed = flip filter notlocked $ \fname -> failedName fname `notElem` existing

    outputListLno' TraceOutput [
                    "Looking for a lock in ", basedir, ". ",
                    "Total number of elements is ", show (length fs),
                    " (not locked: ", show (length notlocked), "; not finished: ", show (length notfinished), ")."]
    -- first try all the elements that are not locked and have not failed
    -- if that fails, try the unlocked but failed
    -- Finally, try the locked elements in the hope that some may be stale
    getLock' basedir notfailed >>= \case
        Just v -> return v
        Nothing -> getLock' basedir (filter (`notElem` notfailed) notlocked) >>= \case
            Just v -> return v
            Nothing -> do
                outputListLno' TraceOutput ["All elements locked. checking for stale locks"]
                getLock' basedir notfinished >>= \case
                    Just v -> return v
                    Nothing -> do
                        let msg = if null (notfailed ++ notfailed)
                                then "All jobs are finished"
                                else "Jobs appear to be running"
                        outputListLno' WarningOutput ["Could get a lock for any file: ", msg]
                        throwError $ NGError NoErrorExit msg

getLock' _ [] = return Nothing
getLock' basedir (f:fs) = do
    let lockname = basedir </> lockName f
    finished <- liftIO $ doesFileExist (basedir </> finishedName f)
    if finished
        then getLock' basedir fs
        else acquireLock LockParameters
                            { lockFname = lockname
                            , maxAge = fromInteger (60*60)
                                -- one hour. Given that lock files are touched
                                -- every ten minutes if things are good (see
                                -- thread below), this is an indication that
                                -- the process has crashed
                            , whenExistsStrategy = IfLockedNothing
                            , mtimeUpdate = True } >>= \case
            Nothing -> getLock' basedir fs
            Just rk -> do
                return $ Just (f,rk)

executeCollect :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeCollect (NGOCounts istream) kwargs = do
    isSubsample <- nConfSubsample <$> nglConfiguration
    current <- lookupStringOrScriptError "collect arguments" "current" kwargs
    allentries <- lookupStringListOrScriptError "collect arguments" "allneeded" kwargs
    ofile <- lookupStringOrScriptError "collect arguments" "ofile" kwargs
    hash <- lookupStringOrScriptError "collect arguments" "__hash" kwargs
    hashdir <- setupHashDirectory "ngless-partials" hash
    (gzfp,gzout) <- openNGLTempFile "compress" "partial." "tsv.gz"
    C.runConduit $
        (snd . asStream $ istream)
        .| CC.concat
        .| CL.map unwrapByteLine
        .| C.unlinesAscii
        .| CAlg.asyncGzipTo gzout
    let partialfile entry = hashdir </> "partial." ++ T.unpack (sanitizePath entry) <.> "tsv.gz"
    outputListLno' TraceOutput ["Collect will write partial file to ", partialfile current]
    liftIO $ do
        hClose gzout
        syncFile gzfp
        moveOrCopy gzfp (partialfile current)
    canCollect <- liftIO $ allM (doesFileExist . partialfile)  (reverse allentries)
                 -- ^ checking in reverse order makes it more likely that ngless notices a missing file early on

    -- It seems wasteful to build the comment string even if `canCollect` is False.
    -- However, these operations are very cheap and provide some basic error checking:
    manualComment <- fmapMaybeM (stringOrTypeError "comment argument to collect() function") (lookup "comment" kwargs)

    autoComments <- case lookup "auto_comments" kwargs of
                        Nothing -> return []
                        Just (NGOList cs) -> mapM (\s -> do
                                                        let errmsg = "auto_comments argument in collect() call"
                                                        symbolOrTypeError errmsg s >>=
                                                            decodeSymbolOrError errmsg
                                                                [("date", AutoDate)
                                                                ,("script", AutoScript)
                                                                ,("hash", AutoResultHash)]) cs
                        _ -> throwScriptError "auto_comments argument to collect() call must be a list of symbols"
    comment <- buildComment manualComment autoComments hash

    if canCollect
        then do
            outputListLno' TraceOutput ["Can collect"]
            newfp <- pasteCounts comment False allentries (map partialfile allentries)
            outputListLno' TraceOutput ["Pasted. Will move result to ", T.unpack ofile]
            moveOrCopyCompress True newfp (T.unpack ofile ++ (if isSubsample then ".subsample" else ""))
        else do
            outputListLno' TraceOutput ["Cannot collect (not all files present yet), wrote partial file to ", partialfile current]
            Just lno <- ngleLno <$> nglEnvironment
            registerHook FinishOkHook $
                outputListLno InfoOutput Nothing
                        ["The collect() call at line ", show lno, " could not be executed as there are partial results missing.\n"
                        ,"When you use the parallel module and the collect() function,\n"
                        ,"you typically need to run ngless *multiple times* (once per sample)!\n"
                        ,"\n\n"
                        ,"For more information, see https://ngless.embl.de/stdlib.html#parallel-module"]
    return NGOVoid
executeCollect arg _ = throwScriptError ("collect got unexpected argument: " ++ show arg)

executeSetTag :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeSetTag (NGOString prefix) [] = do
    liftIO $ writeIORef prefixRef $ T.unpack prefix ++ "-"
    return NGOVoid
executeSetTag arg _ = throwScriptError ("set_parallel_tag got unexpected argument: " ++ show arg)


-- | split a list into a given number of (roughly) equally sized chunks
nChunks :: Int -- ^ number of chunks
            -> [a] -> [[a]]
nChunks 1 xs = [xs]
nChunks n xs = chunksOf p xs
    where
        p = 1 + (length xs `div` n)

splitAtTab ell = case B.elemIndex 9 ell of -- 9 is TAB
    Nothing -> throwDataError "Line does not have a TAB character"
    Just tix -> return $ B.splitAt tix ell

-- partialPaste pastes a set of inputs, returning both the indices (row
-- headers) and the pasted row content
partialPaste :: [V.Vector B.ByteString] -> NGLess (V.Vector B.ByteString, V.Vector B.ByteString)
partialPaste [] = throwShouldNotOccur "partialPaste called with empty vector"
partialPaste vs
    | not (allSame $ V.length <$> vs) = throwDataError $ "collect(): inputs have differing number of rows"
partialPaste (first_ell:ells) = runST $ do
        indices <- VM.new n
        contents <- VM.new n
        fillData 0 indices contents
    where
        n = V.length first_ell
        splitCheck :: Int -> B.ByteString -> V.Vector B.ByteString -> NGLess B.ByteString
        splitCheck !ix !h e
            | B.isPrefixOf h (e V.! ix) = return $! B.drop (B.length h) (e V.! ix)
            | otherwise = throwDataError $
                                "Inconsistent row index in files for collect() [expected index entry '"++B8.unpack h++"', saw '"++B8.unpack (e V.! ix)++"']."
        fillData !ix indices contents
            | ix == n = do
                indices' <- V.unsafeFreeze indices
                contents' <- V.unsafeFreeze contents
                return . Right $ (indices', contents')
            | otherwise = case splitAtTab (first_ell V.! ix) of
                    Left err -> return $ Left err
                    Right (!h,!c) -> case forM ells (splitCheck ix h) of
                        Left err -> return $ Left err
                        Right cs -> do
                            VM.write indices ix h
                            VM.write contents ix $! B.concat (c:cs)
                            fillData (ix + 1) indices contents

concatPartials :: [(V.Vector B.ByteString, V.Vector B.ByteString)] -> NGLess BL.ByteString
concatPartials [] = throwShouldNotOccur "concatPartials of empty set"
concatPartials groups
    | not (allSame (fst <$> groups)) = throwDataError "indices do not match"
    | otherwise = do
        let contents = snd <$> groups
            header = fst (head groups)
        return . BL.fromChunks $ concatMap (\ix -> (header V.! ix):(map (V.! ix) contents ++ ["\n"])) [0 .. V.length header - 1]

-- | strict variation of sinkTBMQueue
sinkTBMQueue' q shouldClose = do
        C.awaitForever $ \ !v -> liftSTM (TQ.writeTBMQueue q v)
        when shouldClose (liftSTM $ TQ.closeTBMQueue q)
    where
        liftSTM = liftIO . atomically

-- If the number of input files is very large (>1024, typically), we risk
-- hitting the limit on open files by a process, so we work in batches of 512.
maxNrOpenFiles = 512 :: Int


{- Now there is a whole lot of complicated code to efficiently solve the
 - following problem:
 -
 - INPUT 0
 -
 -       h0
 - row1 c01
 - row2 c02
 - row3 c03
 -
 - INPUT 1
 -
 -       h1
 - row0 c11
 - row2 c12
 - row4 c14
 -
 - INPUT 2
 -
 -       h2
 - row0 c21
 - row1 c21
 - row3 c23
 -
 - should produce
 -
 - OUTPUT
 -
 -       h0  h1  h2
 - row0 c00 c10 c20
 - row1 c01 c11 c21
 - row2 c02 c12 c22
 - row3 c03 c13 c23
 - row4 c04 c14 c24
 -
 - Where the missing values (e.g., c00) are assumed to be 0
 -
 - A further complication is that each individual input file may itself have
 - multiple columns.
 -}

data SparseCountData = SparseCountData
                            { spdHeader :: !B.ByteString
                            , _spdIndex :: {-# UNPACK #-} !Int
                            , _spdPayload :: !B.ByteString
                            }
    deriving (Eq)

instance Ord SparseCountData where
    compare (SparseCountData ah ai _) (SparseCountData bh bi _) = case compare ah bh of
        EQ -> compare ai bi
        LT -> LT
        GT -> GT

tagSource :: Int -> C.ConduitT ByteLine SparseCountData NGLessIO ()
tagSource ix = C.awaitForever $ \(ByteLine v) -> case splitAtTab v of
    Left err -> lift $ throwError err
    Right (h, pay) -> C.yield $ SparseCountData h ix pay

-- complete takes a set of SparseCountData and fills in missing columns from
-- the placeholder set.
--
-- Example
--      placeholder = [p_0, p_1, p_2, p_3, p_4]
--      hinput = ...
--      inputs = [h_0 0 l_0, h_2 2 l_2, h_3 3 l_3]
--
--      output = [l_0, p_1, l_2, l_3, p_4]
--
complete :: [B.ByteString] -> (SparseCountData,[SparseCountData]) -> ByteLine
complete placeholders (hinput,inputs) = ByteLine $ B.concat merged
    where
        header = spdHeader hinput
        merged = header:complete' 0 placeholders (hinput:inputs)
        complete' _ [] [] = []
        complete' _ [] (_:_) = error "Logic error in StandardModules/parallel//complete"
        complete' ix (p:ps) xs@(SparseCountData _ ix' pay:rest)
            | ix == ix' = pay:complete' (ix+1) ps rest
            | otherwise = p:complete' (ix+1) ps xs
        complete' _ ps [] = ps

-- Merge input lines by index (first element). The input lines are assumed to
-- be sorted, but not necessary identical (i.e., some may be missing).
mergeCounts :: [C.ConduitT () ByteLine NGLessIO ()] -> C.ConduitT () ByteLine NGLessIO ()
mergeCounts [] = throwShouldNotOccur "Attempt to merge empty sources"
mergeCounts ss = do
        start <- forM ss $ \s -> do
            (s', v) <- lift $ s $$+ (CL.mapM (splitAtTab . unwrapByteLine) .| CC.head)
            case v of
                Nothing -> throwShouldNotOccur "Trying to merge a headerless file"
                Just (_,hs) -> do
                    let p = placeholder (B8.count '\t' hs)
                    return (s', p)
        let (ss', placeholders) = unzip start
            ss'' = map C.unsealConduitT ss'
        CAlg.mergeC [s .| tagSource ix | (s,ix) <- zip ss'' [0..]]
            .| CL.groupOn1 spdHeader
            .| CL.map (complete placeholders)
    where
        placeholder :: Int -> B.ByteString
        placeholder n = B.intercalate "\t" ("":["0" | _ <- [1..n]])


{- There are two modes:
 -
 - 1) The rows match (i.e., row headers are always identical). This is the easy
 - case, and it is like the `paste` command
 -
 - 2) The rows do not match. In this case, NGLess assumes that they are sorted
 - [in "C" locale] and merges them.
 -}
pasteCounts :: [T.Text]
            -- ^ comment text
            -> Bool
            -- ^ whether rows match
            -> [T.Text]
            -- ^ headers
            -> [FilePath]
            -- ^ input files
            -> NGLessIO FilePath
pasteCounts comments matchingRows headers inputs
    | length inputs > maxNrOpenFiles = do
        let current = take maxNrOpenFiles inputs
            currenth = take maxNrOpenFiles headers
            rest = drop maxNrOpenFiles inputs
            resth = drop maxNrOpenFiles headers
        first <- pasteCounts [] matchingRows currenth current
        pasteCounts comments matchingRows (snoc resth $ T.intercalate "\t" currenth) (snoc rest first)
    | otherwise = makeNGLTempFile "collected" "collected.counts." "txt" $ \hout -> do
        C.runConduit (commentC "# " comments .| CB.sinkHandle hout)
        liftIO $ T.hPutStrLn hout (T.intercalate "\t" ("":headers))
        numCapabilities <- liftIO getNumCapabilities
        if matchingRows
            then do
                let sources =
                        [conduitPossiblyCompressedFile f
                            .| CB.lines
                            .| (CC.drop 1 >>
                                C.conduitVector 2048 :: C.ConduitT B.ByteString (V.Vector B.ByteString) (ResourceT IO) ())
                            | f <- inputs]
                    sourcesplits = nChunks numCapabilities sources
                channels <- liftIO $ forM sourcesplits $ \ss -> do
                    ch <- TQ.newTBMQueueIO 4
                    a <- A.async $ C.runConduitRes (C.sequenceSources ss .| CL.map (force . partialPaste) .| sinkTBMQueue' ch True)
                    A.link a
                    return (CA.sourceTBMQueue ch, a)
                C.runConduit $
                    C.sequenceSources (fst <$> channels)
                    .| CAlg.asyncMapEitherC numCapabilities (sequence >=> concatPartials)
                    .| CL.map BB.lazyByteString
                    .| CB.sinkHandleBuilder hout
                forM_ (snd <$> channels) (liftIO . A.wait)
            else C.runConduit $
                    mergeCounts [conduitPossiblyCompressedFile f .|  linesC | f <- inputs]
                    .| byteLineSinkHandle hout


executePaste :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executePaste (NGOList ifiles) kwargs = do
    outputListLno' WarningOutput ["Calling __paste which is an internal function, exposed for testing only"]
    ofile <- lookupStringOrScriptError "__paste arguments" "ofile" kwargs
    headers <- lookupStringListOrScriptError "__paste arguments" "headers" kwargs
    matchingRows <- lookupBoolOrScriptErrorDef (return False) "__paste arguments" "matching_rows" kwargs
    ifiles' <- forM ifiles (stringOrTypeError "__concat argument")
    newfp <- pasteCounts [] matchingRows headers (map T.unpack ifiles')
    liftIO $ moveOrCopy newfp (T.unpack ofile)
    return NGOVoid
executePaste _ _ = throwScriptError "Bad call to test function __paste"

lock1 = Function
    { funcName = FuncName "lock1"
    , funcArgType = Just (NGList NGLString)
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

collectFunction = Function
    { funcName = FuncName "collect"
    , funcArgType = Just NGLCounts
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs =
        [ArgInformation "current" True NGLString []
        ,ArgInformation "allneeded" True (NGList NGLString) []
        ,ArgInformation "ofile" True NGLString [ArgCheckFileWritable]
        ,ArgInformation "__can_move" False NGLBool []
        ,ArgInformation "comment" False NGLString []
        ,ArgInformation "auto_comments" False (NGList NGLSymbol) [ArgCheckSymbol ["date", "script", "hash"]]
        ]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

setTagFunction = Function
    { funcName = FuncName "set_parallel_tag"
    , funcArgType = Just NGLCounts
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }


pasteHiddenFunction = Function
    { funcName = FuncName "__paste"
    , funcArgType = Just (NGList NGLString)
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs =
        [ ArgInformation "ofile" True NGLString [ArgCheckFileWritable]
        , ArgInformation "headers" True (NGList NGLString) []
        , ArgInformation "matching_rows" False NGLBool []
        ]
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

addLockHash :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
addLockHash script = pureTransform addLockHash' script
    where
        addLockHash' :: Expression -> Expression
        addLockHash' (FunctionCall (FuncName "lock1") expr kwargs block) =
            FunctionCall (FuncName "lock1") expr ((Variable "__hash", ConstStr h):kwargs) block
            where
                h = T.pack . MD5.md5s . MD5.Str . show $ map snd script
        addLockHash' e = e



loadModule :: T.Text -> NGLessIO Module
loadModule v
    | v `notElem` ["1.0", "0.6"] = throwScriptError ("The behaviour of the parallel module changed.\n"++
                                    "Only versions 1.0 & 0.6 is now supported (currently attempting to import version '"++T.unpack v++"')")
    | otherwise =
        return def
        { modInfo = ModInfo "stdlib.parallel" "1.0"
        , modFunctions =
            [ lock1
            , collectFunction
            , setTagFunction
            , pasteHiddenFunction
            ]
        , modTransform = addLockHash
        , runFunction = \case
            "lock1" -> executeLock1
            "collect" -> executeCollect
            "set_parallel_tag" -> executeSetTag
            "__paste" -> executePaste
            _ -> error "Bad function name"
        }

