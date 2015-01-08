{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
module Output
    ( OutputType(..)
    , output
    , outputList
    ) where

import Text.Printf
import System.IO
import Data.Time
import System.Console.ANSI
import Control.Monad
import System.Console.CmdArgs.Verbosity (isLoud)

data OutputType = DebugOutput | InfoOutput | WarningOutput | ErrorOutput
    deriving (Show, Eq, Ord)

outputList :: OutputType -> [String] -> IO ()
outputList ot ms = output ot (concat ms)

output :: OutputType -> String -> IO ()
output ot msg = do
    isTerm <- hIsTerminalDevice stdout
    loud <- isLoud
    when (isTerm && (loud || ot /= DebugOutput))
         (putStrLn =<< buildOutput ot msg)

buildOutput ot msg = do
    t <- getZonedTime
    let st = setSGRCode [SetColor Foreground Dull (colorFor ot)]
    return $ printf "%s[%s]: %s" st (show t) msg

colorFor DebugOutput = White
colorFor InfoOutput = Blue
colorFor WarningOutput = Yellow
colorFor ErrorOutput = Red
