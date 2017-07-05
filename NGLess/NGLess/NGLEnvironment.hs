{-# LANGUAGE FlexibleContexts #-}
module NGLess.NGLEnvironment
    ( NGLEnvironment(..)
    , nglEnvironment
    , nglConfiguration
    , updateNglEnvironment
    , updateNglEnvironment'
    , writeAnnotatedScriptTo
    , setQuiet
    ) where

import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import System.AtomicWrite.Writer.Text (atomicWriteFile)
import Data.Time (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef

import NGLess.NGError
import Configuration
import CmdArgs

data NGLEnvironment = NGLEnvironment
                    { ngleVersion :: !T.Text
                    , ngleScriptText :: !T.Text -- ^ The original text of the script
                    , ngleMappersActive :: [T.Text] -- ^ which mappers can be used
                    , ngleTemporaryFilesCreated :: [FilePath] -- ^ list of temporary files created
                    , ngleConfiguration :: NGLessConfiguration
                    } deriving (Show, Eq)

ngle :: IORef NGLEnvironment
{-# NOINLINE ngle #-}
ngle = unsafePerformIO (newIORef $ NGLEnvironment "" "" ["bwa"] [] (error "Configuration not set"))

nglEnvironment :: NGLessIO NGLEnvironment
nglEnvironment = liftIO $ readIORef ngle

updateNglEnvironment' :: (NGLEnvironment -> NGLEnvironment) -> IO ()
updateNglEnvironment' = modifyIORef' ngle

updateNglEnvironment :: (NGLEnvironment -> NGLEnvironment) -> NGLessIO ()
updateNglEnvironment = liftIO . updateNglEnvironment'

writeAnnotatedScriptTo :: FilePath -> NGLessIO ()
writeAnnotatedScriptTo output = liftIO $ do
    sc <- ngleScriptText <$> readIORef ngle
    t <- getZonedTime
    let tformat = "%a %d-%m-%Y %R"
        tstr = formatTime defaultTimeLocale tformat t
        sc' = T.concat [T.concat ["# Script ran at ", T.pack tstr, "\n"], sc]
    atomicWriteFile output sc'


nglConfiguration :: NGLessIO NGLessConfiguration
nglConfiguration = ngleConfiguration <$> nglEnvironment

-- | sets verbosity to Quiet
setQuiet :: NGLessIO ()
setQuiet = updateNglEnvironment (\e -> e { ngleConfiguration = setQuiet' (ngleConfiguration e) })
    where
        setQuiet' c = c { nConfVerbosity = Quiet }
