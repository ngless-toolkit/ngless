{- Copyright 2019 NGLess Authors
 - License: MIT
 -}

module Interpretation.CountFile
    ( executeCountFile
    ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit.Algorithms.Async (withPossiblyCompressedFile)
import           Data.Conduit ((.|))
import           Data.List (sortOn)

import Language

import NGLess
import Output
import NGLess.NGLEnvironment (NGLVersion(..), NGLEnvironment(..), nglEnvironment)
import FileOrStream (FileOrStream(..))
import FileManagement (makeNGLTempFile)
import Utils.Conduit (ByteLine(..), linesC, byteLineSinkHandle)

executeCountFile :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeCountFile (NGOString st) _ = do
    let fname = T.unpack st
    ok <- checkCountFile fname
    (NGOCounts . File) <$> if ok
        then return fname
        else maybeNormalizeCountFile fname
executeCountFile other _ = throwScriptError ("Unexpected argument to countfile(): expected str, got " ++ show other)

checkCountFile :: FilePath -> NGLessIO Bool
checkCountFile fname = withPossiblyCompressedFile fname $ \src ->
        C.runConduit (src .| linesC .| isOrdered0)
    where
        isOrdered0 = C.await >>= \case
                            Nothing -> return True
                            Just p -> isOrdered p

        isOrdered prev = C.await >>= \case
                            Nothing -> return True
                            Just next
                                | prev `tagCompare` next -> isOrdered next
                                | otherwise -> return False

maybeNormalizeCountFile :: FilePath -> NGLessIO FilePath
maybeNormalizeCountFile fname = do
    ver@(NGLVersion majV minV) <- ngleVersion <$> nglEnvironment
    if ver < NGLVersion 1 1
        then do
            outputListLno' WarningOutput ["countfile(): file `", fname, "` is not in the right order.\nIn newer versions of NGLess (1.1 and above), the file is normalized (reordered) to avoid errors later. As NGLess is running in compatibility mode for an earlier version (v", show majV, ".", show minV, "), this normalization will be skipped."]
            return fname
        else normalizeCountFile fname

normalizeCountFile :: FilePath -> NGLessIO FilePath
normalizeCountFile fname = makeNGLTempFile "normalized" fname ".tsv" $ \hout ->
    withPossiblyCompressedFile fname $ \src -> do
        C.runConduit (src .| linesC .| sortContent .| byteLineSinkHandle hout)

sortContent = do
    CC.takeWhile (\(ByteLine line) -> B.null line || B8.head line == '#')
    CC.take 1
    content <- CL.consume
    CL.sourceList (sortOn tag content)

tagCompare :: ByteLine -> ByteLine -> Bool
tagCompare line0 line1 = tag line0 < tag line1

tag :: ByteLine -> B.ByteString
tag = fst . B.break (== 9) . unwrapByteLine -- 9 is TAB

