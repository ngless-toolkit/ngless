{- Copyright 2015-2021 NGLess Authors
 - License: MIT
 -}

{-|
 - # Modules
 -
 - Modules add additional functions, constants, or references to ngless
 -
 - See https://ngless.embl.de/modules.html for user information.
 -
 - The data type 'Module' encapsulates all information about a module.
 -}

module Modules
    ( ArgInformation(..)
    , ArgCheck(..)
    , Function(..)
    , FunctionCheck(..)
    , Reference(..)
    , ExternalReference(..)
    , Module(..)
    , ModInfo(..) -- re-export
    , registerModule
    , loadedModules
    , knownModules
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
    , refAlias :: Maybe T.Text
    , refVersionedName :: T.Text
    , refUrl :: Maybe FilePath
    , refHasGff :: Bool
    , refHasFunctionalMap :: Bool
    } deriving (Eq, Show)


-- | Checks for an argument.
data ArgCheck =
            ArgCheckSymbol [T.Text] -- ^ for symbols, list allowed values
            | ArgCheckFileReadable -- ^ file should be readable
            | ArgCheckFileWritable -- ^ file should be writeable (i.e., this is an output file)
            | ArgCheckMinVersion (Int, Int) -- ^ first version where this argument can be used
            | ArgDeprecated (Int, Int) T.Text -- ^ deprecated since given version
            deriving (Eq,Show)

-- | Checks for a function
data FunctionCheck =
            FunctionCheckMinNGLessVersion (Int, Int) -- ^ first version where this function can be used
            | FunctionCheckReturnAssigned -- ^ Function is pure
            | FunctionCheckNGLVersionIncompatibleChange (Int, Int) -- ^ version when behaviour changed
            deriving (Eq, Show)

-- | Basic information about argument to a function
data ArgInformation = ArgInformation
    { argName :: !T.Text -- ^ argument name
    , argRequired :: !Bool -- ^ whether it is required
    , argType :: !NGLType -- ^  type
    , argChecks :: [ArgCheck] -- ^ what checks should be performed on the argument
    } deriving (Eq, Show)


-- | This represents an ngless function
data Function = Function
    { funcName :: FuncName -- ^ name of function
    , funcArgType :: Maybe NGLType -- ^ if it takes an unnamed argument, what is its type
    , funcArgChecks :: [ArgCheck] -- ^ checks for first argument
    , funcRetType :: NGLType -- ^ what type it returns
    , funcKwArgs :: [ArgInformation] -- ^ what are the keyword arguments
    , funcAllowsAutoComprehension :: Bool -- ^ if true, then calling this function with [funcArgType] should return [funcRetType]
    , funcChecks :: [FunctionCheck]
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
                            return (ExternalPackagedReference (Reference name Nothing vname (Just path) hasGtf hasMap))
                        _ -> ExternalReference
                                <$> o .: "name"
                                <*> o .: "fasta-file"
                                <*> o .:? "gtf-file"
                                <*> o .:? "map-file"


-- | This represents a module (i.e., something which is imported by ngless)
data Module = Module
    { modInfo :: !ModInfo -- ^ name & version
    , modPath :: FilePath
    , modCitations :: [T.Text]
    , modConstants :: [(T.Text, NGLessObject)] -- ^ constants defined in this module
    , modReferences :: [ExternalReference]
    , modFunctions :: [Function] -- ^ functions defined by this module
    , modTransform :: [(Int, Expression)] -> NGLessIO [(Int, Expression)] -- ^ this is called before any interpretation. It can also perform validation by throwing exceptions on error
    , runFunction :: T.Text -> NGLessObject -> KwArgsValues -> NGLessIO NGLessObject -- ^ run a function defined by the module. The first argument is the name of the function
    }

instance Show Module where
    show (Module info p _ cs fs _ _ _) = "Module["++show info++" ("++p++"); constants="++show cs++"; functions="++show fs++"]"

instance Default Module where
    def = Module
        { modInfo = ModInfo "builtin.default" "0.0"
        , modPath = "<internal>"
        , modCitations = []
        , modConstants = []
        , modReferences = []
        , modFunctions = []
        , modTransform = return
        , runFunction = \_ _ _ -> return NGOVoid
        }


-- TODO: This really belongs in nglEnvironment!
loadedModulesRef :: IORef [Module]
{-# NOINLINE loadedModulesRef #-}
loadedModulesRef = unsafePerformIO (newIORef [])

registerModule :: Module -> NGLessIO ()
registerModule m = liftIO $ modifyIORef' loadedModulesRef (m:)

loadedModules :: NGLessIO [Module]
loadedModules = liftIO $ readIORef loadedModulesRef

knownModules :: [T.Text]
knownModules =
    ["example-cmd"

    ,"igc"
    ,"om-rgc"
    ,"DogGutCatalog"
    ,"MouseGutCatalog"
    ,"PigGutCatalog"

    ,"specI"
    ,"motus"
    ]

