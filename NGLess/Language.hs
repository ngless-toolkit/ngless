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
    , NGLType(..)
    , Script(..)
    , function_return_type
    , function_arg_type
    ) where

{- This module defines the internal representation the language -}
import qualified Data.Text as T
import Control.Applicative

newtype Variable = Variable T.Text
    deriving (Eq, Show)

-- | functions are hard coded here
data FuncName = Ffastq | Funique | Fpreprocess | Fsubstrim | Fmap | Fcount | Fwrite | Fprint
    deriving (Eq, Show, Ord)

function_argtype_return_type :: [(FuncName, (NGLType, NGLType))]
function_argtype_return_type =
    [(Ffastq,       (NGLReadSet,         NGLFilename))
    ,(Funique,      (NGLReadSet,         NGLReadSet))
    ,(Fpreprocess,  (NGLReadSet,         NGLVoid))
    ,(Fsubstrim,    (NGLRead,            NGLRead))
    ,(Fmap,         (NGLReadSet,         NGLMappedReadSet))
    ,(Fcount,       (NGLMappedReadSet,   NGLCounts))
    ,(Fwrite,       (NGLVoid,            NGLVoid))
    ,(Fprint,       (NGLVoid,            NGLVoid))
    ]

function_return_type f = pure fst <$> (f `lookup` function_argtype_return_type)
function_arg_type f = pure snd <$> (f `lookup` function_argtype_return_type)

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
        | NGOFilename T.Text
        | NGOShortRead T.Text T.Text

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

-- | Script is a version declaration followed by an Expression
-- (almost surely a 'Sequence')
data Script = Script
        { nglVersion :: (Integer, Integer)
        , nglBody :: Expression
        } deriving (Eq,Show)

