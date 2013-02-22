{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Language
    ( Expression(..)
    , Variable(..)
    , UOp(..)
    , BOp(..)
    , Index(..)
    , Block(..)
    , FuncName(..)
    ) where

{- This module defines the internal representation the language -}
import qualified Data.Text as T

newtype Variable = Variable T.Text
    deriving (Eq, Show)
data FuncName = Ffastq | Funique | Fpreprocess | Fsubstrim | Fmap | Fcount | Fwrite | Fprint
    deriving (Eq, Show)

data UOp = UOpLen | UOpMinus
    deriving (Eq, Show)
data BOp = BOpAdd | BOpMul | BOpGT | BOpGTE | BOpLT | BOpLTE | BOpEQ | BOpNEQ
    deriving (Eq, Show)

data Index = Index (Maybe Expression) (Maybe Expression)
    deriving (Eq, Show)

data Block = Block
                [Variable] -- ^ input arguments
                Expression -- ^ block body
    deriving (Eq, Show)

data NGLessBaseType =
        NGLString
        | NGLInteger
        | NGLBool
        | NGLSymbol
        | NGLFilename
        | NGLShortRead
        | NGLShortReadSet
        | NGLMappedRead
        | NGLMappedReadSet
    deriving (Eq, Show)

data NGLessType = NGLType NGLessBaseType | NGLList NGLessBaseType
    deriving (Eq, Show)

data NGLessObject =
        NGOString T.Text
        | NGOBool Bool
        | NGOInteger Integer
        | NGOSymbol T.Text
        | NGOFilename T.Text
        | NGOShortRead T.Text T.Text

data Expression = 
        Lookup Variable -- ^ This is just the variable
        | ConstStr T.Text -- ^ Constant string
        | ConstNum Integer -- ^ integer
        | ConstBool Bool -- ^ true/false
        | ConstSymbol T.Text -- ^ a symbol
        | Continue -- ^ continue
        | Discard -- ^ discard
        | UnaryOp UOp Expression  -- ^ op ( expr )
        | BinaryOp BOp Expression Expression -- ^ expr bop expr
        | Condition Expression Expression Expression -- ^ if condition: true-expr else: false-expr
        | IndexExpression Expression Index -- ^ expr [ index ]
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName [Expression] [(Variable, Expression)] (Maybe Block)
        | Sequence [Expression]
        | NGLessVersion Integer Integer
    deriving (Eq, Show)

