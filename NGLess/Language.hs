{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}

module Language
    ( Expression(..)
    , OptimizedExpression(..)
    , Variable(..)
    , UOp(..)
    , BOp(..)
    , Index(..)
    , Block(..)
    , FuncName(..)
    , MethodName(..)
    , NGLType(..)
    , ReadSet(..)
    , Header(..)
    , ModInfo(..)
    , Script(..)
    , NGLessObject(..)
    , recursiveAnalyse
    , recursiveTransform
    , usedVariables
    , staticValue
    , evalBinary
    ) where

{- This module defines the internal representation the language -}
import qualified Data.Text as T
import           Data.Either.Extra (eitherToMaybe)
import           Control.Monad.Extra (whenJust)
import           Control.Monad.Writer
import           System.FilePath ((</>))

import Data.FastQ
import Data.Sam
import NGLess.NGError
import FileOrStream

newtype Variable = Variable T.Text
    deriving (Eq, Ord, Show)

newtype FuncName = FuncName { unwrapFuncName :: T.Text }
    deriving (Eq, Ord)

instance Show FuncName where
    show (FuncName f) = T.unpack f

newtype MethodName = MethodName { unwrapMethodName :: T.Text }
    deriving (Eq, Ord, Show)

-- | unary operators
data UOp = UOpLen | UOpMinus | UOpNot
    deriving (Eq, Ord, Show)

-- | binary operators
data BOp = BOpAdd | BOpMul | BOpGT | BOpGTE | BOpLT | BOpLTE | BOpEQ | BOpNEQ | BOpPathAppend
    deriving (Eq, Ord, Show)

-- | index expression encodes what is inside an index variable
-- either [a] (IndexOne) or [a:b] (IndexTwo)
data Index = IndexOne Expression
            | IndexTwo (Maybe Expression) (Maybe Expression)
    deriving (Eq, Ord, Show)

-- | a block is
--  f(a) using |inputvariables|:
--      expression
data Block = Block
                { blockVariable :: Variable -- ^ input argument
                , blockBody :: Expression -- ^ block body, will likely be Sequence
                }
    deriving (Eq, Ord, Show)

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
        | NGLSequenceSet
        | NGLCounts
        | NGLVoid
        | NGLAny
        | NGList !NGLType
    deriving (Eq, Ord, Show)


data NGLessObject =
        NGOString !T.Text
        | NGOBool !Bool
        | NGOInteger !Integer
        | NGODouble !Double
        | NGOSymbol !T.Text
        | NGOFilename !FilePath
        | NGOShortRead !ShortRead
        | NGOReadSet T.Text ReadSet
        | NGOSequenceSet FileOrStream
        | NGOMappedReadSet
                    { nglgroupName :: T.Text
                    , nglSamFile :: FileOrStream
                    , nglReference :: Maybe T.Text
                    }
        | NGOMappedRead [SamLine]
        | NGOCounts FileOrStream
        | NGOVoid
        | NGOList [NGLessObject]
    deriving (Eq, Show)


-- | 'Expression' is the main type for holding the AST.

data Expression =
        Lookup (Maybe NGLType) Variable -- ^ This looks up the variable name
        | ConstStr !T.Text -- ^ constant string
        | ConstInt !Integer -- ^ integer
        | ConstDouble !Double -- ^ integer
        | ConstBool !Bool -- ^ true/false
        | ConstSymbol !T.Text -- ^ a symbol
        | BuiltinConstant !Variable -- ^ built-in constant
        | ListExpression [Expression] -- ^ a list
        | Continue -- ^ continue
        | Discard -- ^ discard
        | UnaryOp UOp Expression  -- ^ op ( expr )
        | BinaryOp BOp Expression Expression -- ^ expr bop expr
        | Condition Expression Expression Expression -- ^ if condition: true-expr else: false-expr
        | IndexExpression Expression !Index -- ^ expr [ index ]
        | Assignment Variable Expression -- ^ var = expr
        | FunctionCall FuncName Expression [(Variable, Expression)] (Maybe Block)
        | MethodCall MethodName Expression (Maybe Expression) [(Variable, Expression)] -- ^ expr.method(expre)
        | Sequence [Expression]
        | Optimized OptimizedExpression -- This is a special case, used internally
    deriving (Eq, Ord)

data OptimizedExpression =
        LenThresholdDiscard Variable BOp Int -- if len(r) <op> <int>: discard
        | SubstrimReassign Variable Int -- r = substrim(r, min_quality=<int>)
    deriving (Eq, Ord, Show)

instance Show Expression where
    show (Lookup (Just t) (Variable v)) = "Lookup '"++T.unpack v++"' as "++show t
    show (Lookup Nothing (Variable v)) = "Lookup '"++T.unpack v++"' (type unknown)"
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
    show (UnaryOp op a) = show op ++ "(" ++ show a ++ ")"
    show (BinaryOp op a b) = "BinaryOp" ++ "(" ++ show a ++ " -" ++ show op ++ "- " ++ show b ++ ")"
    show (Condition c a b) = "if ["++show c ++"] then {"++show a++"} else {"++show b++"}"
    show (IndexExpression a ix) = show a ++ "[" ++ show ix ++ "]"
    show (Assignment (Variable v) a) = T.unpack v++" = "++show a
    show (FunctionCall fname a args block) = show fname ++ "(" ++ show a ++ showArgs args ++ ")"
                                    ++ (case block of
                                        Nothing -> ""
                                        Just b -> "using {"++show b ++ "}")
    show (MethodCall mname self a args) = "(" ++ show self ++ ")." ++ show mname ++ "( " ++ show a ++ showArgs args ++ " )"
    show (Sequence e) = "Sequence " ++ show e
    show (Optimized se) = "Optimized (" ++ show se ++ ")"

showArgs [] = ""
showArgs ((Variable v, e):args) = "; "++T.unpack v++"="++show e++showArgs args

{-- Extract static (ie, constant) values from expressions, if possible -}
staticValue :: Expression -> Maybe NGLessObject
staticValue (ConstStr s) = Just $ NGOString s
staticValue (ConstInt v) = Just $ NGOInteger v
staticValue (ConstBool b) = Just $ NGOBool b
staticValue (ConstSymbol s) = Just $ NGOSymbol s
staticValue (BinaryOp bop e1 e2) = do
    v1 <- staticValue e1
    v2 <- staticValue e2
    eitherToMaybe $ evalBinary bop v1 v2
staticValue (ListExpression e) = NGOList <$> mapM staticValue e
staticValue _ = Nothing

asDouble :: NGLessObject -> NGLess Double
asDouble (NGODouble d) = return d
asDouble (NGOInteger i) = return $ fromIntegral i
asDouble other = throwScriptError ("Expected numeric value, got: " ++ show other)

-- Binary Evaluation
evalBinary :: BOp ->  NGLessObject -> NGLessObject -> Either NGError NGLessObject
evalBinary BOpAdd (NGOInteger a) (NGOInteger b) = Right $ NGOInteger (a + b)
evalBinary BOpAdd (NGOString a) (NGOString b) = Right $ NGOString (T.concat [a, b])
evalBinary BOpAdd a b = (NGODouble .) . (+) <$> asDouble a <*> asDouble b
evalBinary BOpMul (NGOInteger a) (NGOInteger b) = Right $ NGOInteger (a * b)
evalBinary BOpMul a b = (NGODouble .) . (+) <$> asDouble a <*> asDouble b
evalBinary BOpPathAppend a b = case (a,b) of
    (NGOString pa, NGOString pb) -> return . NGOString $! T.pack (T.unpack pa </> T.unpack pb)
    _ -> throwShouldNotOccur ("Operator </>: invalid arguments" :: String)

evalBinary BOpEQ (NGOString a) (NGOString b) = return . NGOBool $! a == b
evalBinary BOpNEQ (NGOString a) (NGOString b) = return . NGOBool $! a /= b
evalBinary op a b = do
        a' <- asDouble a
        b' <- asDouble b
        return . NGOBool $ cmp op a' b'
    where
        cmp BOpLT = (<)
        cmp BOpGT = (>)
        cmp BOpLTE = (<=)
        cmp BOpGTE = (>=)
        cmp BOpEQ = (==)
        cmp BOpNEQ = (/=)
        cmp _ = error "should never occur"



-- 'recursiveAnalyse f e' will call the function 'f' for all the subexpression inside 'e'
recursiveAnalyse :: (Monad m) => (Expression -> m ()) -> Expression -> m ()
recursiveAnalyse f e = f e >> recursiveAnalyse' e
    where
        rf = recursiveAnalyse f
        recursiveAnalyse' (ListExpression es) = mapM_ rf es
        recursiveAnalyse' (UnaryOp _ eu) = rf eu
        recursiveAnalyse' (BinaryOp _ e1 e2) = rf e1 >> rf e2
        recursiveAnalyse' (Condition cE tE fE) = rf cE >> rf tE >> rf fE
        recursiveAnalyse' (IndexExpression ei ix) = rf ei >> recurseIndex ix
        recursiveAnalyse' (Assignment _ ea) =  rf ea
        recursiveAnalyse' (FunctionCall _ ef args block) = rf ef >> mapM_ rf (snd <$> args) >> maybe (return ()) (rf . blockBody) block
        recursiveAnalyse' (MethodCall _ em eargs args) = rf em >> maybe (return ()) rf eargs >> mapM_ rf (snd <$> args)
        recursiveAnalyse' (Sequence es) =  mapM_ rf es
        recursiveAnalyse' _ = return ()
        recurseIndex (IndexOne ix) = rf ix
        recurseIndex (IndexTwo ix1 ix2) = whenJust ix1 rf >> whenJust ix2 rf

-- 'recursiveTransform' calls 'f' for every sub-expression in its argument,
-- 'f' will get called with expression where the sub-expressions have already been replaced!
recursiveTransform :: (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
recursiveTransform f e = f =<< recursiveTransform' e
    where
        rf = recursiveTransform f
        recursiveTransform' (ListExpression es) = ListExpression <$> mapM rf es
        recursiveTransform' (UnaryOp op eu) = UnaryOp op <$> rf eu
        recursiveTransform' (BinaryOp op e1 e2) = BinaryOp op <$> rf e1 <*> rf e2
        recursiveTransform' (Condition cE tE fE) = Condition <$> rf cE <*> rf tE <*> rf fE
        recursiveTransform' (IndexExpression ei ix) = flip IndexExpression ix <$> rf ei
        recursiveTransform' (Assignment v ea) = Assignment v <$> rf ea
        recursiveTransform' (FunctionCall fname ef args block) = FunctionCall fname
                                    <$> rf ef
                                    <*> forM args (\(n,av) -> (n,) <$> rf av)
                                    <*> forM block (\(Block vars body) -> Block vars <$> rf body)
        recursiveTransform' (MethodCall mname em earg args) = MethodCall mname
                                    <$> rf em
                                    <*> forM earg rf
                                    <*> forM args (\(n, av) -> (n,) <$> rf av)
        recursiveTransform' (Sequence es) = Sequence <$> mapM rf es
        recursiveTransform' esimple = return esimple

usedVariables :: Expression -> [Variable]
usedVariables expr = execWriter . flip recursiveAnalyse expr $ \case
    (Lookup _ v) -> tell [v]
    _ -> return ()

data ModInfo = ModInfo
    { modName :: !T.Text
    , modVersion :: !T.Text
    } | LocalModInfo
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

