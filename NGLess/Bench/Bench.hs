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

f1 = "../500MB.fq"
f2 = "../2GB.fq"
f3 = "../5GB.fq"

s1 = "../500MB.SAM"
s2 = "../2GB.SAM"
s3 = "../5GB.SAM"

gffHg = NGOString "../hg19.gtf.gz"

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
ms fp = NGOMappedReadSet fp Nothing

remNGLessObj :: NGLessObject -> IO ()
remNGLessObj (NGOReadSet x _ _)     = removeFile x
remNGLessObj (NGOMappedReadSet x _) = removeFile x
remNGLessObj (NGOAnnotatedSet x)    = removeFile x
remNGLessObj x = error ("Shouldn't have happened: " ++ show x)


execAndRmTFiles obj = obj >>= remNGLessObj >> return obj

main = do
    odir <- outputDirectory scriptFName
    createDirectoryIfMissing False odir
    let [qc1, qc2, qc3] = map (evS . executeQualityProcess)  $ map NGOString [T.pack f1, T.pack f2, T.pack f3]
        [u1, u2, u3]    = map (\x -> evS $ executeUnique x [])              $ map rs [f1,f2,f3]
        [qp1, qp2, qp3] = map (\x -> evS $ executePreprocess x [] block "") $ map rs [f1,f2,f3]
        [m1, m2, m3]    = map (\x -> evS $ executeMap x [refsacCer3])       $ map rs [f1,f2,f3]
        -- load sam files directly --
        [an1, an2, an3] = map (\x -> evS $ executeAnnotation x [("gff", gffHg)]) $ map ms [s1,s2,s3]
    defaultMain [ 
        bgroup "fastq"
            [
                bench "500MB" (whnfIO $ qc1),
                bench "2GB"   (whnfIO $ qc2),
                bench "5GB"   (whnfIO $ qc3)
            ],
        bgroup "unique"
            [
                bench "500MB" ( whnfIO . execAndRmTFiles $ u1 ),
                bench "2GB"   ( whnfIO . execAndRmTFiles $ u2 ),
                bench "5GB"   ( whnfIO . execAndRmTFiles $ u3 )
            ],
        bgroup "preprocess"
            [
                bench "100MB" ( whnfIO . execAndRmTFiles $ qp1 ),
                bench "2GB"   ( whnfIO . execAndRmTFiles $ qp2 ),
                bench "5GB"   ( whnfIO . execAndRmTFiles $ qp3 )
            ],
        bgroup "map"
            [
                bench "100MB" ( whnfIO . execAndRmTFiles $ m1 ),
                bench "2GB"   ( whnfIO . execAndRmTFiles $ m2 ),
                bench "5GB"   ( whnfIO . execAndRmTFiles $ m3 )
            ],
        bgroup "annotate"
            [
                bench "100MB" ( whnfIO . execAndRmTFiles $ an1 ),
                bench "2GB"   ( whnfIO . execAndRmTFiles $ an2 ),
                bench "5GB"   ( whnfIO . execAndRmTFiles $ an3 )
            ]
      ]

