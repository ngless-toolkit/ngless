{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

{-# LANGUAGE TupleSections, OverloadedStrings #-}

module StandardModules.Parallel
    ( loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath.Posix
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import System.IO
import Data.Default
import Control.Monad
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile)

import qualified Data.Hash.MD5 as MD5
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import           Data.Conduit ((=$=), ($$))

import Hooks
import Output
import NGLess
import Modules
import Language
import Transform
import FileManagement

import Utils.Utils
import Utils.Conduit
import Utils.LockFile

setupHashDirectory :: FilePath -> T.Text -> NGLessIO FilePath
setupHashDirectory basename hash = do
    let actiondir = basename </> take 8 (T.unpack hash)
    liftIO $ createDirectoryIfMissing True actiondir
    return actiondir

executeLock1 (NGOList entries) kwargs  = do
    entries' <- mapM (stringOrTypeError "lock1") entries
    hash <- lookupStringOrScriptError "lock1" "__hash" kwargs
    lockdir <- setupHashDirectory "ngless-lock" hash
    (e,rk) <- getLock lockdir entries'
    registerHook FinishOkHook $ do
        let receiptfile = lockdir </> (T.unpack e) ++ ".finished"
        liftIO $ withFile receiptfile WriteMode $ \h ->
            hPutStrLn h (concat ["Finished ", T.unpack e])
        release rk
    return $! NGOString e

executeLock1 arg _ = throwScriptError ("Wrong argument for lock1 (expected a list of strings, got `" ++ show arg ++ "`")


getLock _ [] = do
   outputListLno' InfoOutput ["Could get a lock for any file."]
   throwGenericError "Could not obtain any lock"
getLock basedir (x:xs) = do
    let fname = T.unpack x
    finished <- liftIO $ doesFileExist (basedir </> fname ++ ".finished")
    if finished
        then getLock basedir xs
        else acquireLock (basedir </> fname ++ ".lock") >>= \case
            Nothing -> getLock basedir xs
            Just rk -> return (x,rk)

partialfile :: T.Text -> FilePath
partialfile entry = "ngless-partials" </> T.unpack entry <.> "part"

executeCollect :: NGLessObject -> [(T.Text, NGLessObject)] -> NGLessIO NGLessObject
executeCollect (NGOCounts countfile) kwargs = do
    current <- lookupStringOrScriptError "collect arguments" "current" kwargs
    allentries <- lookupStringListOrScriptError "collect arguments" "allneeded" kwargs
    ofile <- lookupStringOrScriptError "collect arguments" "ofile" kwargs
    canMove <- lookupBoolOrScriptErrorDef (return True) "collect hidden argument" "__can_move" kwargs
    hash <- lookupStringOrScriptError "lock1" "__hash" kwargs
    lockdir <- setupHashDirectory "ngless-partials" hash

    liftIO $ (if canMove then moveOrCopy else copyFile) countfile (partialfile current)
    canCollect <- all id <$> forM allentries (\e -> liftIO $ doesFileExist (partialfile e))
    when canCollect $
        concatCounts allentries (map partialfile allentries) (T.unpack ofile)
    return NGOVoid
executeCollect arg _ = throwScriptError ("collect got unexpected argument: " ++ show arg)

concatlines :: [[B.ByteString]] -> NGLess B.ByteString
concatlines [] = return B.empty
concatlines entries
    | any null entries = throwDataError "Empty line in collect"
    | allSame (head <$> entries) = return $! flip B8.snoc '\n' (B8.intercalate "\t" ((head . head $ entries):concat (tail <$> entries)))
    | otherwise = throwDataError "Mismatched collect()"

concatCounts :: [T.Text] -> [FilePath] -> FilePath -> NGLessIO ()
concatCounts headers inputs ofile = do
    (newfp,hout) <- openNGLTempFile ofile "collected.counts." "txt"
    liftIO $ T.hPutStrLn hout (T.intercalate "\t" ("":headers))
    C.sequenceSources
        [conduitPossiblyCompressedFile f
            =$= CB.lines
            =$= (C.await >> C.awaitForever C.yield) -- drop header line
            =$= CL.map (B8.split '\t')
            | f <- inputs]
        =$= CL.mapM (runNGLess . concatlines)
        $$ C.sinkHandle hout
    liftIO $ do
        hClose hout
        moveOrCopy newfp ofile


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
addHash script = pureTransform (addHash' hash) script
    where
        hash :: T.Text
        hash = T.pack . MD5.md5s . MD5.Str . show . map snd $ script
        addHash' :: T.Text -> Expression -> Expression
        addHash' h (FunctionCall (FuncName fn) expr kwargs block)
            | fn `elem` ["lock1", "collect"] = FunctionCall (FuncName fn) expr ((Variable "__hash", ConstStr h):kwargs) block
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

