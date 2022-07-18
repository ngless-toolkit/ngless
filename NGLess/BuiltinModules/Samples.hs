{- Copyright 2022 NGLess Authors
 - License: MIT
 -}

module BuiltinModules.Samples
    ( loadModule
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Default (def)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Types ((.:), (.:?))
import           Data.Either (lefts, rights)
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Vector as V
import           Control.Monad.IO.Class (liftIO)
import           System.FilePath ((</>), isAbsolute)


import Language

import Data.FastQ
import Modules
import NGLess

data SampleData = SampleData
    { sampleName :: !T.Text
    , sampleData :: ReadSet
    } deriving (Eq, Ord, Show)
    -- equivalent to NGOReadSet

data SampleFile = SampleFile
    { sfBasedir :: Maybe FilePath
    , sfSamples :: [SampleData]
    } deriving (Eq, Ord, Show)


instance Aeson.FromJSON SampleFile where
    parseJSON = Aeson.withObject "SampleFile" $ \ob -> do
        samples <- ob .: "samples"
        SampleFile
            <$> ob .:? "basedir"
            <*> parseSamples samples

parseSamples :: Aeson.Value -> Aeson.Parser [SampleData]
parseSamples = Aeson.withObject "samples" $ \ss ->
    mapM parseSample (Aeson.toList ss)

parseSample :: (Aeson.Key, Aeson.Value) -> Aeson.Parser SampleData
parseSample (name, elems) = do
    data_ <- parseSamplePaths (Aeson.toText name) elems
    return $ SampleData (Aeson.toText name) data_

parseSamplePaths :: T.Text -> Aeson.Value -> Aeson.Parser ReadSet
parseSamplePaths name = Aeson.withArray "paths" $ \arr -> do
    paths <- mapM (parseSamplePath name) (V.toList arr)
    return $ ReadSet
                (map (\(a,b) ->
                        (FastQFilePath SangerEncoding a, FastQFilePath SangerEncoding b)
                        ) $ lefts paths)
                (map (FastQFilePath SangerEncoding) $ rights paths)

parseSamplePath :: T.Text -> Aeson.Value -> Aeson.Parser (Either (FilePath, FilePath) FilePath)
parseSamplePath name = Aeson.withObject "path" $ \ob -> do
    case Aeson.toList ob of
        [("paired", elems)] -> flip (Aeson.withArray "paired") elems $ \v ->
            case V.toList v of
                [Aeson.String fq1, Aeson.String fq2] -> return $ Left (T.unpack fq1, T.unpack fq2)
                _ -> fail ("Invalid paired sample '" ++ T.unpack name ++ "'")
        [("single", elems)] -> flip (Aeson.withArray "single") elems $ \v ->
            case V.toList v of
                [Aeson.String fq1] -> return . Right $ T.unpack fq1
                _ -> fail ("Invalid sample '" ++ T.unpack name ++ "'")
        _ -> fail ("Invalid sample '" ++ T.unpack name ++ "'")

normalize :: SampleFile -> [NGLessObject]
normalize (SampleFile maybeBasedir samples) =
        map norm1 samples
    where
        norm1 :: SampleData -> NGLessObject
        norm1 (SampleData name data_) =
                NGOReadSet name (maybeAddBasedir maybeBasedir data_)

        addBaseDir1 :: FilePath -> FastQFilePath -> FastQFilePath
        addBaseDir1 basedir f@(FastQFilePath enc fq)
            | isAbsolute fq = f
            | otherwise = FastQFilePath enc $ basedir </> fq

        maybeAddBasedir :: Maybe FilePath -> ReadSet -> ReadSet
        maybeAddBasedir Nothing data_ = data_
        maybeAddBasedir (Just basedir) (ReadSet pairs singles) =
            ReadSet
                (map (\(a,b) -> (addBaseDir1 basedir a, addBaseDir1 basedir b)) pairs)
                (map (addBaseDir1 basedir) singles)

executeLoadSampleList :: NGLessObject -> KwArgsValues -> NGLessIO NGLessObject
executeLoadSampleList (NGOString fname) _ = do
    Yaml.decodeEither' <$> liftIO (B.readFile $ T.unpack fname) >>= \case
        Right sf -> return $ NGOList $ normalize sf
        Left err -> throwSystemError ("Could not sample information file "++ (T.unpack fname) ++ ". Error was `" ++ show err ++ "`")
executeLoadSampleList arg _ = throwShouldNotOccur ("load_sample_list called with argument: " ++ show arg)


load_sample_list_Function = Function
    { funcName = FuncName "load_sample_list"
    , funcArgType = Just NGLString
    , funcArgChecks = []
    , funcRetType = NGList NGLReadSet
    , funcKwArgs = []
    , funcAllowsAutoComprehension = True
    , funcChecks = [FunctionCheckMinNGLessVersion (1,5)
                   ,FunctionCheckReturnAssigned]
    }

loadModule :: T.Text -> NGLessIO Module
loadModule _ = return def
    { modInfo = ModInfo "builtin.samples" "1.5"
    , modFunctions = [load_sample_list_Function]
    , runFunction = \case
                        "load_sample_list" -> executeLoadSampleList
                        _ -> error "NOT POSSIBLE"
    }
