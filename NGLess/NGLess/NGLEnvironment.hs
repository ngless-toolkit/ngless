{- Copyright 2016-2022 NGLess Authors
 - License: MIT
 -}
module NGLess.NGLEnvironment
    ( NGLVersion(..)
    , parseVersion
    , NGLEnvironment(..)
    , nglEnvironment
    , nglConfiguration
    , updateNglEnvironment
    , updateNglEnvironment'
    , setQuiet
    , registerModule

    , Hook(..)
    , registerHook
    , registerFailHook
    , triggerFailHook
    , triggerHook

    , setModulesForTestingPurposesOnlyDoNotUseOtherwise
    , setupTestEnvironment
    ) where

import qualified Data.Text as T

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import Modules (Module(..))
import NGLess.NGError
import Configuration
import CmdArgs (Verbosity(..))

data NGLVersion = NGLVersion !Int !Int
                        deriving (Eq, Show)

instance Ord NGLVersion where
    compare (NGLVersion majV0 minV0) (NGLVersion majV1 minV1)
        | majV0 == majV1 = compare minV0 minV1
        | otherwise = compare majV0 majV1

data Hook = FinishOkHook
    deriving (Eq, Show, Ord, Bounded, Enum)

data NGLEnvironment = NGLEnvironment
                    { ngleVersion :: !NGLVersion
                    , ngleLno :: !(Maybe Int)
                    , ngleScriptText :: !T.Text -- ^ The original text of the script
                    , ngleMappersActive :: [T.Text] -- ^ which mappers can be used
                    , ngleTemporaryFilesCreated :: [FilePath] -- ^ list of temporary files created
                    , ngleConfiguration :: NGLessConfiguration
                    , ngleLoadedModules :: [Module]
                    , ngleActiveHooks :: [(Hook, NGLessIO ())]
                    }

parseVersion :: Maybe T.Text -> NGLess NGLVersion
parseVersion Nothing = return $ NGLVersion 1 5
parseVersion (Just "1.5") = return $ NGLVersion 1 5
parseVersion (Just "1.4") = return $ NGLVersion 1 4
parseVersion (Just "1.3") = return $ NGLVersion 1 3
parseVersion (Just "1.2") = return $ NGLVersion 1 2
parseVersion (Just "1.1") = return $ NGLVersion 1 1
parseVersion (Just "1.0") = return $ NGLVersion 1 0
parseVersion (Just "0.0") = return $ NGLVersion 0 0
parseVersion (Just "0.5") = return $ NGLVersion 0 5
parseVersion (Just "0.6") = return $ NGLVersion 0 6
parseVersion (Just "0.7") = return $ NGLVersion 0 7
parseVersion (Just "0.8") = return $ NGLVersion 0 8
parseVersion (Just "0.9") = return $ NGLVersion 0 9
parseVersion (Just "0.10") = return $ NGLVersion 0 10
parseVersion (Just "0.11") = return $ NGLVersion 0 11
parseVersion (Just v) = case T.splitOn "." v of
                            [majV,minV,_] ->
                                throwScriptError $ concat ["The NGLess version string at the top of the file should only\ncontain a major and a minor version, separated by a dot.\n\n"
                                                        ,"You probably meant to write:\n\n"
                                                        ,"ngless \"" , T.unpack majV, ".", T.unpack minV, "\"\n"]
                            [_, _] -> throwScriptError $ concat ["Version ", T.unpack v, " is not supported (only versions 1.[0-5] and 0.0/0.5-12 are available in this release)."]
                            _ -> throwScriptError $ concat ["Version ", T.unpack v, " could not be understood. The version string should look like \"1.0\" or similar"]
ngle :: IORef NGLEnvironment
{-# NOINLINE ngle #-}
ngle = unsafePerformIO (newIORef $ NGLEnvironment (NGLVersion 0 0) Nothing "" ["bwa"] [] (error "Configuration not set") [] [])


failHooks :: IORef [IO ()]
{-# NOINLINE failHooks #-}
failHooks = unsafePerformIO (newIORef [])


nglEnvironment :: NGLessIO NGLEnvironment
nglEnvironment = liftIO $ readIORef ngle

updateNglEnvironment' :: (NGLEnvironment -> NGLEnvironment) -> IO ()
updateNglEnvironment' = modifyIORef' ngle

updateNglEnvironment :: (NGLEnvironment -> NGLEnvironment) -> NGLessIO ()
updateNglEnvironment = liftIO . updateNglEnvironment'

nglConfiguration :: NGLessIO NGLessConfiguration
nglConfiguration = ngleConfiguration <$> nglEnvironment

registerModule :: Module -> NGLessIO ()
registerModule m = updateNglEnvironment $ \env ->
    env { ngleLoadedModules = m:ngleLoadedModules env }

setModulesForTestingPurposesOnlyDoNotUseOtherwise :: [Module] -> NGLessIO ()
setModulesForTestingPurposesOnlyDoNotUseOtherwise mods = updateNglEnvironment $ \env -> env { ngleLoadedModules = mods }

registerHook :: Hook -> NGLessIO () -> NGLessIO ()
registerHook hook act = updateNglEnvironment $ \env ->
    env { ngleActiveHooks = (hook, act):ngleActiveHooks env }

{- Run if the script fails. Note that these hooks are in the IO Monad, not
 - NGLessIO! -}
registerFailHook :: IO () -> NGLessIO ()
registerFailHook act = liftIO $ modifyIORef failHooks (act:)

-- Run all fail hooks
triggerFailHook :: IO ()
triggerFailHook = readIORef failHooks >>= sequence_

-- Trigger the actions registered with the given hook
triggerHook :: Hook -> NGLessIO ()
triggerHook hook = do
    registered <- ngleActiveHooks <$> nglEnvironment
    forM_ registered $ \(h,act) ->
        when (h == hook) act


-- | sets verbosity to Quiet
setQuiet :: NGLessIO ()
setQuiet = updateNglEnvironment (\e -> e { ngleConfiguration = setQuiet' (ngleConfiguration e) })
    where
        setQuiet' c = c { nConfVerbosity = Quiet, nConfPrintHeader = False }

-- | setup an environment that can be used for testing
setupTestEnvironment :: IO ()
setupTestEnvironment = do
    config <- guessConfiguration
    let config' = config { nConfTemporaryDirectory = "testing_tmp_dir", nConfKeepTemporaryFiles = True, nConfVerbosity = Quiet }
    updateNglEnvironment' (\env -> env { ngleConfiguration = config' })
