{- Copyright 2013 NGLess Authors
 - License: GPL, version 3 or later
 -}
module Language
    ( Expression(..)
    , Variable(..)
    , UOp(..)
    , BOp(..)
    , FuncName(..)
    ) where

{- This module defines the internal representation the language -}
import Data.Maybe
import qualified Data.ByteString as S

newtype Variable = Variable S.ByteString
    deriving (Eq, Show)
data FuncName = Ffastq | Fsubstrim | Fmap | Fcount | Fwrite | Fprint
    deriving (Eq, Show)

data UOp = UOpLen
    deriving (Eq, Show)
data BOp = BOpAdd | BOpMul
    deriving (Eq, Show)

data Index = Index (Maybe Integer) (Maybe Integer)
    deriving (Eq, Show)

data Block = Block
                [Variable] -- ^ input arguments
                [Expression] -- ^ block body
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
        NGOString S.ByteString
        | NGOBool Bool
        | NGOInteger Integer
        | NGOSymbol S.ByteString
        | NGOFilename S.ByteString
        | NGOShortRead S.ByteString S.ByteString

data Expression = 
        Lookup Variable -- ^ This is just the variable
        | ConstStr S.ByteString -- ^ Constant string
        | ConstNum Integer -- ^ integer
        | ConstBool Bool -- ^ true/false
        | ConstSymbol S.ByteString -- ^ a symbol
        | Continue -- ^ continue
        | Discard -- ^ discard
        | UnaryOp UOp Expression  -- ^ op ( expr )
        | BinaryOp BOp Expression Expression -- ^ expr bop expr
        | Condition Expression Expression Expression -- ^ if condition: true-expr else: false-expr
        | IndexExpression Expression Index -- ^ expr [ index ]
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName [Expression] [(Variable, Expression)] (Maybe Block)
        | Sequence [Expression]
    deriving (Eq, Show)

