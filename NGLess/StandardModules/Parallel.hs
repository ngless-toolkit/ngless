{- Copyright 2016-2025 NGLess Authors
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
import qualified Data.Set as S
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
import           Control.Monad
import           Control.Monad.ST (runST)
import           Control.Monad.Except (throwError)
import           Control.Monad.Extra (allM, unlessM)
import           Control.DeepSeq
import           Control.Monad.Trans.Class
import           System.AtomicWrite.Writer.Text (atomicWriteFile)
import           System.Random.Shuffle (shuffleM)


import Control.Monad.Trans.Resource
import Control.Monad.State.Lazy
import System.IO
import Data.Default
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)

import qualified Data.Hash.MD5 as MD5
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((.|), (.|), ($$+))

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

import Interpretation.Write (moveOrCopyCompress, WriteOptions(..))

import Utils.Utils (fmapMaybeM, allSame, moveOrCopy)
import Utils.Conduit
import qualified Utils.LockFile as LockFile
import           Utils.LockFile (LockParameters(..))

syncFile :: FilePath -> IO ()
#ifndef WINDOWS
syncFile fname = do
    bracket (openFd fname ReadWrite defaultFileFlags)
        closeFd
        fileSynchronise
    -- The code below will not work on Windows
    bracket (openFd (takeDirectory fname) ReadOnly defaultFileFlags)
        closeFd
        fileSynchronise

#else
syncFile _ = return ()
#endif


setupHashDirectory :: String -> FilePath -> T.Text -> NGLessIO FilePath
setupHashDirectory prefix basename hash = do
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

executeLock1OrForAll funcname (NGOList entries) kwargs  = do
    let readSetOrTypeError (NGOReadSet name _) = return name
        readSetOrTypeError _ = throwShouldNotOccur "Expected a readset"
    entries' <- case entries of
        [] -> throwDataError "Cannot run on empty list"
        (NGOString _:_) -> mapM (stringOrTypeError funcname) entries
        (NGOReadSet _ _:_) -> mapM (readSetOrTypeError) entries
        _ -> throwScriptError ("Unsupported type for function " ++ funcname)

    hash <- lookupStringOrScriptError funcname "__hash" kwargs
    tag <- lookupStringOrScriptErrorDef (return "") "collect arguments (hidden tag)"
                (if funcname == "lock1" then "__parallel_tag" else "tag") kwargs
    let prefix
            | T.null tag = ""
            | otherwise = T.unpack tag ++ "-"
    lockdir <- setupHashDirectory prefix "ngless-locks" hash
    -- Keep a map of 'sane -> original' names used for locks to backtrace
    -- what file was locked and return the unsanitized name
    -- See also https://github.com/ngless-toolkit/ngless/issues/68
    let saneentries = sanitizePath <$> entries'
    (e,rk) <- getLock lockdir saneentries
    outputListLno' InfoOutput [funcname, ": Obtained lock file: '", lockdir </> T.unpack e ++ ".lock", "'"]
    reportbase <- setupHashDirectory prefix "ngless-stats" hash
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
        withFile logfile WriteMode $ \h -> do
            hPutStrLn h "Execution failed. Execution log:"
            writeOutputTo h
    case entries of
        (NGOString _:_) -> return . NGOString $! fromMaybe e $ lookup e (zip saneentries entries')
        _ -> case lookup e (zip saneentries entries) of
                Just r -> return r
                Nothing -> throwShouldNotOccur "Could not find entry in map (should not happen)"

executeLock1OrForAll func arg _ = throwScriptError ("Wrong argument for " ++ func ++ " (expected a list of strings, got `" ++ show arg ++ "`")


lockName = (++ ".lock") . T.unpack
finishedName = (++ ".finished") . T.unpack
failedName = (++ ".failed") . T.unpack

-- | Create a lock file
getLock :: FilePath
                -- ^  directory where to create locks
                -> [T.Text]
                -- ^ keys to attempt to lock
                -> NGLessIO (T.Text, ReleaseKey)
getLock basedir fs = do
    existing <- liftIO $ S.fromList <$> getDirectoryContents basedir
    let notfinished = flip filter fs $ \fname -> finishedName fname `S.notMember` existing
        notlocked = flip filter notfinished $ \fname -> lockName fname `S.notMember` existing
        notfailed = flip filter notlocked $ \fname -> failedName fname `S.notMember` existing
        failed = flip filter notfinished $ \fname -> failedName fname `S.member` existing
        locked = flip filter notfinished $ \fname -> lockName fname `S.member` existing
    when (null notfinished) $ do
        outputListLno' InfoOutput ["All jobs are finished"]
        throwError $ NGError NoErrorExit "All jobs are finished"

    outputListLno' TraceOutput ["Looking for a lock in '", basedir, "'"]
    outputListLno' TraceOutput [
                    "Total number of tasks to run is ", show (length fs),
                    " (total not finished (including locked & failed): ", show (length notfinished),
                    " ; locked: ", show (length locked),
                    " ; failed: ", show (length failed),
                    ")."]
    -- first try all the tasks that are not locked and have not failed
    -- if that fails, try the locked tasks in the hope that some may be stale
    -- Finally, try the unlocked but failed (in random order)
    getLock' basedir notfailed >>= \case
        Just v -> return v
        Nothing -> do
            outputListLno' InfoOutput ["All tasks locked or failed. Checking for stale locks..."]
            getLock' basedir locked >>= \case
                Just v -> return v
                Nothing -> do
                    when (null failed) $ do
                        outputListLno' InfoOutput ["All jobs appear to be finished or running"]
                        throwError $ NGError NoErrorExit "All jobs are finished or running"
                    -- randomizing the order maximizes the possibilities to get a lock
                    failed' <- liftIO $ shuffleM failed
                    outputListLno' InfoOutput ["All tasks locked or failed and there are no stale locks."]
                    outputListLno' InfoOutput ["Will retry some failed tasks, but it is possible that this will fail again."]
                    outputListLno' InfoOutput ["Failed logs are in directory '", basedir, "'"]
                    getLock' basedir failed' >>= \case
                        Just v -> return v
                        Nothing -> do
                            let msg
                                 | null (notfailed ++ notfailed) = "All jobs are finished"
                                 | null failed = "Jobs appear to be running"
                                 | otherwise = "Jobs are either locked or failed. Check directory '" ++ basedir ++ "' for more information"
                            outputListLno' WarningOutput ["Could get a lock for any file: ", msg]
                            throwError $ NGError NoErrorExit msg

getLock' _ [] = return Nothing
getLock' basedir (f:fs) =
    LockFile.acquireLock LockFile.LockParameters
                        { lockFname = basedir </> lockName f
                        , maxAge = fromInteger (60*60)
                            -- one hour. Given that lock files are touched
                            -- every ten minutes if things are good (see
                            -- thread below), this is an indication that
                            -- the process has crashed
                        , whenExistsStrategy = LockFile.IfLockedNothing
                        , mtimeUpdate = True } >>= \case
        Nothing -> getLock' basedir fs
        Just rk -> do
            isFinished <- liftIO $ doesFileExist (basedir </> finishedName f)
            if isFinished
                then do
                    release rk
                    getLock' basedir fs
                else return $ Just (f, rk)

executeCollect :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeCollect (NGOCounts istream) kwargs = do
    isSubsample <- nConfSubsample <$> nglConfiguration
    current <- case lookup "current" kwargs of
        Nothing -> throwScriptError "current not specified in collect call"
        Just (NGOString c) -> return c
        Just (NGOReadSet n _) -> return n
        Just _ -> throwScriptError "current argument (in collect()) must be a string or a readset"

    allentries <- case lookup "allneeded" kwargs of
        Nothing -> throwScriptError "collect() called without 'allneeded' argument"
        Just (NGOList ells) -> forM ells $ \case
            NGOString s -> return s
            NGOReadSet n _ -> return n
            _ -> throwScriptError "collect() called with 'allneeded' argument, but not all elements are strings or readsets"
        Just _ -> throwScriptError "collect() called with 'allneeded' argument that is not a list"

    ofile <- lookupStringOrScriptError "collect arguments" "ofile" kwargs
    hash <- lookupStringOrScriptError "collect arguments" "__hash" kwargs
    tag <- lookupStringOrScriptErrorDef (return "") "collect arguments (hidden tag)" "__parallel_tag" kwargs
    let prefix
            | T.null tag = ""
            | otherwise = T.unpack tag ++ "-"
    hashdir <- setupHashDirectory prefix "ngless-partials" hash
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
            moveOrCopyCompress (def
                                    { woCompressLevel = Nothing
                                    , woCanMove = True
                                    , woOFile = T.unpack ofile ++ (if isSubsample then ".subsample" else "")
                                    }) newfp
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
executeSetTag _ _ = throwShouldNotOccur "set_parallel_tag should have been transformed away!"


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
concatPartials groups@(g:_)
    | not (allSame (fst <$> groups)) = throwDataError "indices do not match"
    | otherwise = do
        let contents = snd <$> groups
            header = fst g
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
                Nothing -> do
                    lift $ outputListLno' WarningOutput ["Merging empty file"]
                    return (s', placeholder 1)
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
    , funcArgType = Just (NGLUnion [NGList NGLString, NGList NGLReadSet])
    , funcArgChecks = []
    , funcRetType = NGLString
    , funcKwArgs = []
    , funcAllowsAutoComprehension = False
    , funcChecks = []
    }

collectFunction isV11 = Function
    { funcName = FuncName "collect"
    , funcArgType = Just NGLCounts
    , funcArgChecks = []
    , funcRetType = NGLVoid
    , funcKwArgs =
        [ArgInformation "current" (not isV11) NGLString []
        ,ArgInformation "allneeded" (not isV11) (NGList NGLString) []
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
    , funcArgType = Just NGLString
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



runForAllFunctions =
    [ Function
        { funcName = FuncName "run_for_all"
        , funcArgType = Just (NGList NGLString)
        , funcArgChecks = []
        , funcRetType = NGLString
        , funcKwArgs =
            [ ArgInformation "tag" False NGLString []
            ]
        , funcAllowsAutoComprehension = False
        , funcChecks = []
        }
    , Function
        { funcName = FuncName "run_for_all_samples"
        , funcArgType = Just (NGList NGLReadSet)
        , funcArgChecks = []
        , funcRetType = NGLReadSet
        , funcKwArgs =
            [ ArgInformation "tag" False NGLString []
            ]
        , funcAllowsAutoComprehension = False
        , funcChecks = []
        }
    ]


parallelTransform :: Bool -> [(Int, Expression)] -> NGLessIO [(Int, Expression)]
parallelTransform includeForAll = processRunForAll includeForAll >=> processSetParallelTag >=> addLockHash

addLockHash :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
addLockHash script = pureTransform addLockHash' script
    where
        addLockHash' :: Expression -> Expression
        addLockHash' (FunctionCall fn@(FuncName fname) expr kwargs block)
            | fname `elem` ["lock1", "run_for_all", "run_for_all_samples"] =
                FunctionCall fn expr ((Variable "__hash", ConstStr h):kwargs) block
                where
                    h = T.pack . MD5.md5s . MD5.Str . show $ map snd script
        addLockHash' e = e

processRunForAll :: Bool -> [(Int, Expression)] -> NGLessIO [(Int, Expression)]
processRunForAll False = checkNoRunForAll
processRunForAll True = processRunForAll' Nothing

processRunForAll' _ [] = return []
processRunForAll' Nothing ((lno,expr):rest) = case expr of
    Assignment v (FunctionCall (FuncName fname) slist kwargs _)
        | fname `elem` ["run_for_all", "run_for_all_samples"] -> do
            let save_match = Assignment (Variable "$parallel$iterator") (Lookup (Just NGLString) v)
                save_list  = Assignment (Variable "$parallel$list") slist
                set_tag = do
                    tag <- lookup (Variable "tag") kwargs
                    return (lno,
                            FunctionCall (FuncName "set_parallel_tag") tag [] Nothing)
            rest' <- processRunForAll' (Just (lno, slist)) rest
            let res = ((lno,expr):(lno,save_match):(lno,save_list):rest')
            case set_tag of
                Nothing -> return res
                Just t -> return (t:res)
    _ -> do
        ((lno,expr):) <$> processRunForAll' Nothing rest
processRunForAll' (Just prev) ((lno,e):rest) = case e of
    Assignment _ (FunctionCall (FuncName fname) _ _ _)
        | fname `elem` ["run_for_all", "run_for_all_samples"] -> do
            throwScriptError ("The functions 'run_for_all'/'run_for_all_samples' can only be called once (seen on lines "++show prev++" and "++show lno++")")
    FunctionCall fn@(FuncName "collect") expr kwargs block -> do
        let kwargs' =  (Variable "allneeded", Lookup (Just NGLString) (Variable "$parallel$list"))
                      :(Variable "current", Lookup (Just NGLString) (Variable "$parallel$iterator"))
                      :kwargs
            e' = FunctionCall fn expr kwargs' block
        rest' <- processRunForAll' (Just prev) rest
        return ((lno,e'):rest')
    _ -> do
        rest' <- processRunForAll' (Just prev) rest
        return ((lno,e):rest')

checkNoRunForAll = mapM checkNoRunForAll1
    where
        checkNoRunForAll1 (_,Assignment _ (FunctionCall (FuncName fname) _ _ _))
            | fname `elem` ["run_for_all", "run_for_all_samples"] =
                throwScriptError ("Function '"++T.unpack fname++"' is only available in parallel module version 1.1+. Please upgrade your import")
        checkNoRunForAll1 e = return e

processSetParallelTag :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
processSetParallelTag = return . processSetParallelTag' False
    where
        processSetParallelTag' :: Bool -> [(Int, Expression)] -> [(Int, Expression)]
        processSetParallelTag' _ [] = []
        processSetParallelTag' hasTag ((lno, e):rest) = let
                (e',ch) = case e of
                    FunctionCall (FuncName "set_parallel_tag") expr [] Nothing
                            -> (Assignment (Variable "$parallel$tag") expr, True)
                    FunctionCall fn@(FuncName fname) expr kwargs block
                        | hasTag && fname `elem` ["lock1", "collect"]
                            -> (FunctionCall fn expr ((Variable "__parallel_tag", Lookup (Just NGLString) (Variable "$parallel$tag")):kwargs) block, True)
                    _ -> (e, False)
                rest' = processSetParallelTag' (hasTag || ch) rest
            in (lno, e'):rest'


loadModule :: T.Text -> NGLessIO Module
loadModule v
    | v `notElem` ["1.1", "1.0", "0.6"] = throwScriptError ("The behaviour of the parallel module changed.\n"++
                                    "Only versions 1.1/1.0/0.6 are now supported (currently attempting to import version '"++T.unpack v++"')")
    | otherwise = do
        let includeForAll = v == "1.1"
        return def
            { modInfo = ModInfo "stdlib.parallel" v
            , modFunctions =
                [ lock1
                , collectFunction includeForAll
                , setTagFunction
                , pasteHiddenFunction
                ] ++ (if includeForAll then runForAllFunctions else [])
            , modTransform = parallelTransform includeForAll
            , runFunction = \case
                "lock1" -> executeLock1OrForAll "lock1"
                "collect" -> executeCollect
                "set_parallel_tag" -> executeSetTag
                "run_for_all" -> executeLock1OrForAll "run_for_all"
                "run_for_all_samples" -> executeLock1OrForAll "run_for_all_samples"
                "__paste" -> executePaste
                _ -> error "Bad function name"
            }

