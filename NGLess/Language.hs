{- Copyright 2013 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE OverloadedStrings #-}


module Language
    ( Expression(..)
    , Variable(..)
    , UOp(..)
    , BOp(..)
    , Index(..)
    , Block(..)
    , FuncName(..)
    , NGLType(..)
    , Script(..)
    , NGLessObject(..)
    , function_opt_arg_type
    , function_return_type
    , function_arg_type
    ) where

{- This module defines the internal representation the language -}
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import Data.FastQ

newtype Variable = Variable T.Text
    deriving (Eq, Show)

-- | functions are hard coded here
data FuncName = Ffastq | Funique | Fpreprocess | Fsubstrim | Fmap | Fcount | Fwrite | Fprint | Fannotate
    deriving (Eq, Show)

function_argtype_return_type :: FuncName -> (NGLType, NGLType)
function_argtype_return_type Ffastq =       (NGLReadSet,         NGLString)
function_argtype_return_type Funique =      (NGLReadSet,         NGLReadSet)
function_argtype_return_type Fpreprocess =  (NGLVoid,            NGLReadSet)
function_argtype_return_type Fsubstrim =    (NGLRead,            NGLRead)
function_argtype_return_type Fmap =         (NGLMappedReadSet,   NGLReadSet)
function_argtype_return_type Fcount =       (NGLCounts,          NGLMappedReadSet)
function_argtype_return_type Fannotate =    (NGLMappedReadSet,   NGLMappedReadSet)
function_argtype_return_type err = error ("Function " ++ (show err) ++ " shouldn't reach this")
--function_argtype_return_type Fwrite =       (NGLVoid,            NGLVoid)
--function_argtype_return_type Fprint =       (NGLVoid,            NGLVoid)

function_return_type :: FuncName -> NGLType
function_return_type = fst . function_argtype_return_type

function_arg_type :: FuncName -> NGLType
function_arg_type = snd . function_argtype_return_type


function_opt_arg_type :: FuncName -> Variable -> Either T.Text NGLType
function_opt_arg_type Funique     (Variable "max_copies")  = Right NGLInteger
function_opt_arg_type Fmap        (Variable "reference")   = Right NGLString
function_opt_arg_type Fannotate   (Variable "gff")         = Right NGLString
function_opt_arg_type Fannotate   (Variable "mode")        = Right NGLSymbol
function_opt_arg_type Fannotate   (Variable "features")    = Right $ NGList NGLSymbol
function_opt_arg_type Fannotate   (Variable "ambiguity")   = Right NGLSymbol
function_opt_arg_type Fannotate   (Variable "strand")      = Right NGLSymbol
function_opt_arg_type Fcount      (Variable "counts")      = Right $ NGList NGLSymbol
function_opt_arg_type Fcount      (Variable "min")         = Right NGLInteger
function_opt_arg_type Fsubstrim   (Variable "min_quality") = Right NGLInteger
function_opt_arg_type Fwrite      (Variable "ofile")       = Right NGLString
function_opt_arg_type Fwrite      (Variable "format")      = Right NGLSymbol
function_opt_arg_type Fwrite      (Variable "verbose")     = Right NGLSymbol
function_opt_arg_type Ffastq       _ = Left "Fastq function does not have any argument"
function_opt_arg_type Fpreprocess  _ = Left "Preprocess function does not have any argument"
function_opt_arg_type e (Variable x) = Left $ T.concat ["Function " ,T.pack . show $ e ," does not have argument: ", x]


-- | unary operators
data UOp = UOpLen | UOpMinus
    deriving (Eq, Show)

-- | binary operators
data BOp = BOpAdd | BOpMul | BOpGT | BOpGTE | BOpLT | BOpLTE | BOpEQ | BOpNEQ
    deriving (Eq, Show)

-- | index expression encodes what is inside an index variable
-- either [a] (IndexOne) or [a:b] (IndexTwo)
data Index = IndexOne Expression
            | IndexTwo (Maybe Expression) (Maybe Expression)
    deriving (Eq, Show)

-- | a block is
--  f(a) using |inputvariables|:
--      expression
data Block = Block
                [Variable] -- ^ input arguments
                Expression -- ^ block body, will likely be Sequence
    deriving (Eq, Show)

data NGLType =
        NGLString
        | NGLInteger
        | NGLBool
        | NGLSymbol
        | NGLFilename
        | NGLRead
        | NGLReadSet
        | NGLMappedRead
        | NGLMappedReadSet
        | NGLCounts
        | NGLVoid
        | NGList NGLType
    deriving (Eq, Show)

data NGLessObject =
        NGOString T.Text
        | NGOBool Bool
        | NGOInteger Integer
        | NGOSymbol T.Text
        | NGOFilename FilePath
        | NGOShortRead ShortRead
        | NGOReadSet FilePath FastQEncoding B.ByteString -- ^ Read sets are saved on disk
        | NGOMappedReadSet FilePath (Maybe T.Text) -- ^ This is represented by a SAM file on disk + optional reference information
        | NGOAnnotatedSet FilePath
        | NGOVoid
        | NGOList [NGLessObject]
    deriving (Eq, Show, Ord)


-- | 'Expression' is the main type for holding the AST.

data Expression =
        Lookup Variable -- ^ This looks up the variable name
        | ConstStr T.Text -- ^ constant string
        | ConstNum Integer -- ^ integer
        | ConstBool Bool -- ^ true/false
        | ConstSymbol T.Text -- ^ a symbol
        | ListExpression [Expression] -- ^ a list
        | Continue -- ^ continue
        | Discard -- ^ discard
        | UnaryOp UOp Expression  -- ^ op ( expr )
        | BinaryOp BOp Expression Expression -- ^ expr bop expr
        | Condition Expression Expression Expression -- ^ if condition: true-expr else: false-expr
        | IndexExpression Expression Index -- ^ expr [ index ]
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)] (Maybe Block)
        | Sequence [Expression]
    deriving (Eq, Show)

-- | Script is a version declaration followed by a series of expressions
data Script = Script
        { nglVersion :: T.Text
        , nglBody :: [(Int,Expression)] -- ^ (line number, expression)
        } deriving (Eq,Show)

