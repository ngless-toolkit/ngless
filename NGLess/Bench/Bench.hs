{-# LANGUAGE OverloadedStrings #-}

import Interpret
import Validation
import ValidationNotPure
import Language
import Tokens
import Types
import Parse
import WebServer
import Configuration
import ReferenceDatabases
import Data.FastQ

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Criterion.Main

import System.Console.CmdArgs
import System.Directory


import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S


scriptFName = "test.ngl"

f1 = "../sample_1.fq"
f2 = "../sample_1.fq"
refsacCer3 = ("reference", NGOString "sacCer3")


evS f = evalStateT (runErrorT f) m >>= \x -> case x of
        Left  _ -> error ("Shouldn't have happend.")
        Right a -> return a 
    where 
        nglessScript = NGOString "NOP"
        nglessScriptFname = NGOFilename scriptFName
        m = (0, M.insert ".scriptfname" nglessScriptFname (M.insert ".script" nglessScript M.empty))
        
{- 
    pre process block
    
    (1) remove last bp 
    (2) substrim
-}

block = Block [Variable "read"] expr
expr  = Sequence [assign index, assign subs]

index = IndexExpression lkup (IndexTwo Nothing (Just (BinaryOp BOpAdd (UnaryOp UOpLen lkup) (UnaryOp UOpMinus (ConstNum 1)))))
subs  = FunctionCall Fsubstrim lkup [(Variable "min_quality",ConstNum 5)] Nothing

assign= Assignment (Variable "read") 
lkup = Lookup (Variable "read")

--

rs fp = NGOReadSet fp SolexaEncoding ""



main = do
    odir <- outputDirectory scriptFName
    createDirectoryIfMissing False odir
    let [qc1, qc2] = map (evS . executeQualityProcess)    $ map NGOString [T.pack f1, T.pack f2]
        [u1, u2]   = map (\x -> evS $ executeUnique x []) $ map rs [f1,f2]
        [qp1, qp2] = map (\x -> evS $ executePreprocess x [] block "") $ map rs [f1,f2]
        [m1, m2]   = map (\x -> evS $ executeMap x [refsacCer3]) $ map rs [f1,f2]
        [an1, an2] = map (\x -> x >>= \y -> evS $ executeAnnotation y []) [m1, m2]
    defaultMain [ 
        bgroup "fastqFunction"
            [
                bench "100M" (whnfIO qc1),
                bench "5GB"  (whnfIO qc2)
            ],
        bgroup "unique"
            [
                bench "100MB" ( whnfIO u1 ),
                bench "5GB"   ( whnfIO u2 )
            ],
        bgroup "pre-process"
            [
                bench "100MB" ( whnfIO qp1 ),
                bench "5GB"   ( whnfIO qp2 )
            ],
        bgroup "map"
            [
                bench "100MB" ( whnfIO m1 ),
                bench "5GB"   ( whnfIO m2 )
            ],
        bgroup "annotate"
            [
                bench "100MB" ( whnfIO an1 ),
                bench "5GB"   ( whnfIO an2 )
            ]
      ]

