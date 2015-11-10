{- Copyright 2013-2015 NGLess Authors
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
    , MethodName(..)
    , NGLType(..)
    , Header(..)
    , ModInfo(..)
    , Script(..)
    , NGLessObject(..)
    , methodSelfType
    , methodArgType
    , methodReturnType
    , methodKwargType
    , typeOfConstant
    ) where

{- This module defines the internal representation the language -}
import qualified Data.Text as T

import Data.FastQ
import Data.Sam

newtype Variable = Variable T.Text
    deriving (Eq, Show)

newtype FuncName = FuncName T.Text
    deriving (Eq)

instance Show FuncName where
    show (FuncName f) = T.unpack f

data MethodName =
        Mflag
        | Mpe_filter
    deriving (Eq, Show)


-- | method name -> ((method self type, method first argtype if any), method return type)
methodArgTypeReturnType :: MethodName -> ((NGLType, Maybe NGLType), NGLType)
methodArgTypeReturnType Mflag = ((NGLMappedRead, Just NGLSymbol), NGLBool)
methodArgTypeReturnType Mpe_filter = ((NGLMappedRead, Nothing), NGLMappedRead)

methodSelfType :: MethodName -> NGLType
methodSelfType = fst . fst . methodArgTypeReturnType

methodArgType :: MethodName -> (Maybe NGLType)
methodArgType = snd . fst. methodArgTypeReturnType

methodReturnType :: MethodName -> NGLType
methodReturnType = snd . methodArgTypeReturnType

methodKwargType :: MethodName -> Variable -> NGLType
methodKwargType _ _ = NGLVoid

typeOfConstant :: T.Text -> Maybe NGLType
typeOfConstant "STDIN"        = Just NGLString
typeOfConstant "STDOUT"       = Just NGLString
typeOfConstant _              = Nothing

-- | unary operators
data UOp = UOpLen | UOpMinus | UOpNot
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
        | NGLDouble
        | NGLBool
        | NGLSymbol
        | NGLFilename
        | NGLRead
        | NGLReadSet
        | NGLMappedRead
        | NGLMappedReadSet
        | NGLAnnotatedSet
        | NGLCounts
        | NGLVoid
        | NGLAny
        | NGList NGLType
    deriving (Eq, Show)

data NGLessObject =
        NGOString T.Text
        | NGOBool Bool
        | NGOInteger Integer
        | NGODouble Double
        | NGOSymbol T.Text
        | NGOFilename FilePath
        | NGOShortRead ShortRead
        | NGOReadSet1 FastQEncoding FilePath -- ^ encoding file_on_disk
        | NGOReadSet2 FastQEncoding FilePath FilePath -- ^ encoding file_on_disk
        | NGOReadSet3 FastQEncoding FilePath FilePath FilePath-- ^ encoding file_on_disk
        | NGOMappedReadSet FilePath (Maybe T.Text) -- ^ This is represented by a SAM file on disk + optional reference information
        | NGOMappedRead [SamLine]
        | NGOAnnotatedSet FilePath FilePath -- ^ annotated_reads headers
        | NGOCounts FilePath
        | NGOVoid
        | NGOList [NGLessObject]
    deriving (Eq, Show, Ord)


-- | 'Expression' is the main type for holding the AST.

data Expression =
        Lookup Variable -- ^ This looks up the variable name
        | ConstStr T.Text -- ^ constant string
        | ConstInt Integer -- ^ integer
        | ConstDouble Double -- ^ integer
        | ConstBool Bool -- ^ true/false
        | ConstSymbol T.Text -- ^ a symbol
        | BuiltinConstant Variable -- ^ built-in constant
        | ListExpression [Expression] -- ^ a list
        | Continue -- ^ continue
        | Discard -- ^ discard
        | UnaryOp UOp Expression  -- ^ op ( expr )
        | BinaryOp BOp Expression Expression -- ^ expr bop expr
        | Condition Expression Expression Expression -- ^ if condition: true-expr else: false-expr
        | IndexExpression Expression Index -- ^ expr [ index ]
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)] (Maybe Block)
        | MethodCall MethodName Expression (Maybe Expression) [(Variable, Expression)] -- ^ expr.method(expre)
        | Sequence [Expression]
    deriving (Eq)

instance Show Expression where
    show (Lookup (Variable v)) = "Lookup '"++T.unpack v++"'"
    show (ConstStr t) = show t
    show (ConstInt n) = show n
    show (ConstDouble f) = show f
    show (ConstBool b) = show b
    show (ConstSymbol t) = "{"++T.unpack t++"}"
    show (BuiltinConstant (Variable v)) = T.unpack v
    show (ListExpression e) = show e
    show Continue = "continue"
    show Discard = "discard"
    show (UnaryOp UOpLen a) = "len("++show a++")"
    show (UnaryOp op a) = show op ++ " " ++ show a
    show (BinaryOp op a b) = show a ++ show op ++ show b
    show (Condition c a b) = "if ["++show c ++"] then {"++show a++"} else {"++show b++"}"
    show (IndexExpression a ix) = show a ++ "[" ++ show ix ++ "]"
    show (Assignment (Variable v) a) = T.unpack v++" = "++show a
    show (FunctionCall fname a args block) = show fname ++ "(" ++ show a ++ showArgs args ++ ")"
                                    ++ (case block of
                                        Nothing -> ""
                                        Just b -> "using {"++show b ++ "}")
    show (MethodCall mname self a args) = "(" ++ show self ++ ")." ++ show mname ++ "( " ++ show a ++ showArgs args ++ " )"
    show (Sequence e) = "Sequence " ++ show e

showArgs [] = ""
showArgs ((Variable v, e):args) = "; "++T.unpack v++"="++show e++showArgs args

data ModInfo = ModInfo
    { modName :: !T.Text
    , modVersion :: !T.Text
    } deriving (Eq, Show)

data Header = Header
    { nglVersion :: T.Text
    , nglModules :: [ModInfo]
    } deriving (Eq, Show)

-- | Script is a version declaration followed by a series of expressions
data Script = Script
    { nglHeader :: Maybe Header -- ^ optional if -e option is used
    , nglBody :: [(Int,Expression)] -- ^ (line number, expression)
    } deriving (Eq,Show)

