{- Copyright 2015-2016 NGLess Authors
 - License: MIT
 -}
module Modules
    ( ArgInformation(..)
    , ArgCheck(..)
    , Function(..)
    , Reference(..)
    , ExternalReference(..)
    , Module(..)
    , ModInfo(..) -- re-export
    , registerModule
    , loadedModules
    ) where
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Aeson
import Data.Default

import Language
import NGLess

data Reference = Reference
    { refName :: T.Text
    , refVersionedName :: T.Text
    , refUrl :: Maybe FilePath
    , refHasGff :: Bool
    , refHasFunctionalMap :: Bool
    } deriving (Eq, Show)

data ArgCheck =
            ArgCheckSymbol [T.Text] -- ^ for symbols, list allowed values
            | ArgCheckFileReadable -- ^ file should be readable
            | ArgCheckFileWritable -- ^ file should be writeable (i.e., this is an output file)
            deriving (Eq,Show)

-- | Basic information about argument to a function
data ArgInformation = ArgInformation
    { argName :: !T.Text -- ^ argument name
    , argRequired :: !Bool -- ^ whether it is required
    , argType :: !NGLType -- ^  type
    , argChecks :: [ArgCheck] -- ^ what checks should be performed on the argument
    } deriving (Eq, Show)

data Function = Function
    { funcName :: FuncName -- ^ name of function
    , funcArgType :: Maybe NGLType -- ^ if it takes an unnamed argument, what is its type
    , funcRetType :: NGLType -- ^ what type it returns
    , funcKwArgs :: [ArgInformation] -- ^ what are the keyword arguments
    , funcAllowsAutoComprehension :: Bool -- ^ if true, then calling this function with [funcArgType] should return [funcRetType]
    } deriving (Eq, Show)

data ExternalReference = ExternalReference
        { erefName :: T.Text
        , faFile :: FilePath
        , gtfFile :: Maybe FilePath
        , geneMapFile :: Maybe FilePath
        }
        | ExternalPackagedReference
        { refData :: Reference
        }
    deriving (Eq, Show)

instance FromJSON ExternalReference where
    parseJSON = withObject "external reference" $ \o -> do
                    rtype <- o .:? "rtype"
                    case (rtype :: Maybe String) of
                        Just "packaged" -> do
                            name <- o .: "name"
                            vname <- o .: "name-version"
                            path <- o .: "url"
                            hasGtf <- o .: "has-gtf"
                            hasMap <- o .: "has-mapfile"
                            return (ExternalPackagedReference (Reference name vname (Just path) hasGtf hasMap))
                        _ -> ExternalReference
                                <$> o .: "name"
                                <*> o .: "fasta-file"
                                <*> o .:? "gtf-file"
                                <*> o .:? "map-file"

data Module = Module
    { modInfo :: !ModInfo -- ^ name & version
    , modCitation :: Maybe T.Text
    , modConstants :: [(T.Text, NGLessObject)] -- ^ constants defined in this module
    , modReferences :: [ExternalReference]
    , modFunctions :: [Function] -- ^ functions defined by this module
    , modTransform :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
    , runFunction :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject -- ^ run a function defined by the module. The first argument is the name of the function
    , validateFunction :: [(Int,Expression)] -> NGLessIO [T.Text] -- ^ this is called before any interpretation, should return a list of error messages (empty if no errors)
    }

instance Show Module where
    show (Module info _ cs fs _ _ _ _) = "Module["++show info++"; constants="++show cs++"; functions="++show fs++"]"

instance Default Module where
    def = Module
        { modInfo = ModInfo "builtin.default" "0.0"
        , modCitation = Nothing
        , modConstants = []
        , modReferences = []
        , modFunctions = []
        , modTransform = return
        , runFunction = \_ _ _ -> return NGOVoid
        , validateFunction = const (return [])
        }


loadedModulesRef :: IORef [Module]
{-# NOINLINE loadedModulesRef #-}
loadedModulesRef = unsafePerformIO (newIORef [])

registerModule :: Module -> NGLessIO ()
registerModule m = liftIO $ modifyIORef' loadedModulesRef (m:)

loadedModules :: NGLessIO [Module]
loadedModules = liftIO $ readIORef loadedModulesRef
