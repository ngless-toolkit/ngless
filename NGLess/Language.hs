module NGLess.Language
    ( Expression(..)
    ) where

import Data.Maybe
import qualified Data.ByteString as S

newtype Variable = Variable S.ByteString
    deriving (Eq, Show)
data FuncName = Ffastq | Fsubstream | Fmap | Fcount | Fwrite | Fprint
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
        Lookup Variable
        | ConstStr S.ByteString
        | ConstNum Integer
        | ConstBool Bool
        | ConstSymbol S.ByteString
        | Continue
        | Discard
        | UnaryOp UOp Expression 
        | BinaryOp BOp Expression Expression
        | Condition Expression Expression Expression
        | IndexExpression Expression Index
        | Assignment Variable Expression
        | FunctionCall FuncName [Expression] [(Variable, Expression)] (Maybe Block)
        | Sequence [Expression]
    deriving (Eq, Show)

