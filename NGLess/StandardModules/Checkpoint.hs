{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module StandardModules.Checkpoint
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
import           System.Posix.Unistd (fileSynchronise)
import           System.Posix.IO (openFd, defaultFileFlags, closeFd, OpenMode(..), handleToFd)
import           System.FilePath.Posix
import           GHC.Conc (getNumCapabilities, atomically)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TBMQueue as TQ
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TQueue as CA
import           Control.Monad.ST
import           Control.DeepSeq
import           Data.List (find)
import           Data.Traversable

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
import FileManagement

import Utils.Utils
import Utils.Conduit
import Utils.LockFile

checkpointTransform :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
checkpointTransform script = forM script $ \(ix,e) -> do
        e' <- recursiveTransform (replaceCheckpoint ix) e
        return (ix, e')
    where
        replaceCheckpoint :: Int -> Expression -> NGLessIO Expression
        replaceCheckpoint ix
            (FunctionCall fn@(FuncName "checkpoint") expr kwargs block) = do
                hash <- hashExpression <$> buildTree ix expr
                let fname = "ngless-checkpoints" </> take 24 hash ++ ".sam"
                liftIO $ createDirectoryIfMissing True "ngless-checkpoints"
                exists <- liftIO $ doesFileExist fname
                return $ if exists
                   then FunctionCall (FuncName "samfile") (ConstStr $ T.pack fname) [] Nothing
                   else FunctionCall (FuncName "write") expr [(Variable "oname", T.pack fname)] Nothing
        replaceCheckpoint _ e = return e

        hashExpression :: Expression -> String
        hashExpression = MD5.md5s . MD5.Str . show
        buildTree :: Int -> Expression -> NGLessIO Expression
        buildTree cur = recursiveTransform buildTree'
            where
                buildTree' (Lookup var) =
                    case findAssignment cur var of
                      Just (ix, Assignment _ expr) -> return $ buildTree ix expr
                      Nothing -> throwShouldNotOccur ("Could not find a top-level assignment to variable '"++T.unpack var++"'.\n\tCheckpointing is only supported by straight code-path values.")
                buildTree' e = return e
        findAssignment :: Int -> Variable -> Expression
        findAssignment start var = find match revscript
            where
                revscript = reverse . map snd . filter ((< start) . fst) $ script
                match :: Expression -> Bool
                match e@(Assignment (Lookup var') _) = var == var'
                match _ = False

loadModule :: T.Text -> NGLessIO Module
loadModule _ =
        return def
        { modInfo = ModInfo "stdlib.checkpoint" "0.0"
        , modTransform = checkpointTransform
        }

