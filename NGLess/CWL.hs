{- Copyright 2017 NGLess Authors
 - License: MIT
 -}
module CWL
    ( writeCWL
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           Control.Monad.Trans.Cont (evalCont, callCC)

import Utils.Utils
import Language

{- Build a CWL wrapper for the NGLess script
 -
 - Currently, this is very simplistic with little error checking.
 -}


build :: FilePath -> [Integer] -> Integer -> String
build sfname inputs output = concat
    ["cwlVersion: cwl:draft-3\n"
    ,"class: CommandLineTool\n"
    ,"baseCommand: [ngless, ", sfname, "]\n"
    ,"inputs:\n"
    ,buildInputs inputs
    ,builtOutput output
    ]

buildInputs :: [Integer] -> String
buildInputs = concatMap buildInput
buildInput :: Integer -> String
buildInput p = concat
    ["- id: input", show p, "\n"
    ,"  type: Str\n"
    ,"  inputBinding:\n"
    ,"    position: ", show p, "\n"
    ]
builtOutput p = concat
    ["outputs:\n"
    ,"  -id: nglessout\n"
    ,"  type: File\n"
    ,"  outputBinding:\n"
    ,"    glob: $(inputs.input"++show p++")\n"
    ]


extractARGVUsage :: Expression -> Maybe Integer
extractARGVUsage e = evalCont $ callCC $ \exit -> do
                        recursiveAnalyse (extractARGVUsage' exit) e
                        return Nothing
    where
        extractARGVUsage' exit (IndexExpression (Lookup _ (Variable "ARGV")) (IndexOne (ConstInt ix1))) = exit (Just ix1)
        extractARGVUsage' _ _ = return ()

extractAllARGVUsage :: Script -> [Integer]
extractAllARGVUsage (Script _ body) = mapMaybe extractARGVUsage (snd <$> body)

extractOutput (Script _ body) = head $ mapMaybe  extractOutput' (snd <$> body)
    where
        extractOutput' :: Expression -> Maybe Integer
        extractOutput' (FunctionCall (FuncName "write") _ kwargs _) = do
            ofile <- lookup (Variable "ofile") kwargs
            extractARGVUsage ofile
        extractOutput' _ = Nothing

writeCWL :: Script -> FilePath -> FilePath -> IO ()
writeCWL sc scFname fp =
    withOutputFile fp $ \h ->
        B.hPut h (B8.pack $ buildCWL scFname sc)

buildCWL :: FilePath -> Script -> String
buildCWL scFname sc = build scFname (extractAllARGVUsage sc) (extractOutput sc)
