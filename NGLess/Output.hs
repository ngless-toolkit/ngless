{- Copyright 2013-2015 NGLess Authors
 - License: MIT
 -}
module Output
    ( OutputType(..)
    , outputLno'
    , outputListLno
    , outputListLno'
    , setOutputLno
    ) where

import Text.Printf
import System.IO
import System.IO.Unsafe
import Data.IORef
import Data.Time
import System.Console.ANSI
import Control.Monad
import System.Console.CmdArgs.Verbosity

data OutputType = DebugOutput | InfoOutput | ResultOutput | WarningOutput | ErrorOutput
    deriving (Show, Eq, Ord)

curLine :: IORef (Maybe Int)
{-# NOINLINE curLine #-}
curLine = unsafePerformIO (newIORef Nothing)

setOutputLno = writeIORef curLine

outputListLno :: OutputType -> Maybe Int -> [String] -> IO ()
outputListLno ot Nothing ms = output ot (concat ms)
outputListLno ot (Just lno) ms = output ot (concat $ ["Line ", show lno, ": "] ++ ms)

outputListLno' ot ms = do
    lno <- readIORef curLine
    outputListLno ot lno ms

outputLno' :: OutputType -> String -> IO ()
outputLno' ot m = outputListLno' ot [m]

output :: OutputType -> String -> IO ()
output ot msg = do
    isTerm <- hIsTerminalDevice stdout
    verb <- getVerbosity
    when (isTerm && (verb == Loud || ot >= InfoOutput) && (verb /= Quiet || ot >= ResultOutput)) $ do
        t <- getZonedTime
        let st = setSGRCode [SetColor Foreground Dull (colorFor ot)]
        putStrLn $ printf "%s[%s]: %s" st (show t) msg

colorFor DebugOutput = White
colorFor InfoOutput = Blue
colorFor ResultOutput = Black
colorFor WarningOutput = Yellow
colorFor ErrorOutput = Red
