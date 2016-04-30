{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module Transform
    ( transform
    , pureTransform
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Cont
import Control.Monad.Writer
import Control.Arrow (second)
import Control.Monad.Identity (runIdentity)

import Language
import Modules
import Output
import NGLess


transform :: [Module] -> Script -> NGLessIO Script
transform mods sc = Script (nglHeader sc) <$> applyM transforms (nglBody sc)
    where
        applyM [] e = return e
        applyM (t:ts) e = t e >>= applyM ts
        transforms = modTransforms ++ builtinTransforms
        modTransforms = map modTransform mods
        builtinTransforms =
                [ writeToMove
                , qcInPreprocess
                , ifLenDiscardSpecial
                , substrimReassign
                ]

pureRecursiveTransform :: (Expression -> Expression) -> Expression -> Expression
pureRecursiveTransform f e = runIdentity (recursiveTransform (return . f) e)

pureTransform :: (Expression -> Expression) -> [(Int,Expression)] -> NGLessIO [(Int, Expression)]
pureTransform f = return . map (second (pureRecursiveTransform f))

addArgument :: T.Text -- ^ function name
            -> (Variable, Expression) -- ^ new argument
            -> Expression -- ^ expression to transform
            -> Expression -- ^ transformed expression
addArgument func newArg expr = case expr of
    Assignment v e -> Assignment v (addArgument func newArg e)
    FunctionCall fname@(FuncName fname') e args b
        | fname' == func ->
            FunctionCall fname e (newArg:args) b
    _ -> expr

isVarUsed :: Variable -> [(Int, Expression)] -> Bool
isVarUsed v = any (isVarUsed1 v . snd)

isVarUsed1 :: Variable -> Expression -> Bool
isVarUsed1 v expr = evalCont $ callCC $ \exit -> do
                recursiveAnalyse (isVarUsed1' exit) expr
                return False
    where
        isVarUsed1' :: (Bool -> Cont Bool ()) -> Expression -> Cont Bool ()
        isVarUsed1' exit (Lookup v')
            | v == v' = exit True
        isVarUsed1' _ _ = return ()

writeToMove :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
writeToMove = return . writeToMove'
writeToMove' [] = []
writeToMove' ((lno,expr):rest) = (lno, addMove toRemove expr):writeToMove' rest
    where
        toRemove = filter (not . flip isVarUsed rest) $ functionVars "write" expr

-- | Variables used in calling the function func
functionVars :: T.Text -- ^ function name
                -> Expression -- expression to analyse
                -> [Variable]
functionVars fname expr = execWriter (recursiveAnalyse fvars expr)
    where
        fvars :: Expression -> Writer [Variable] ()
        fvars (FunctionCall (FuncName fname') (Lookup v) _ _)
            | fname' == fname = tell [v]
        fvars _ = return ()

addMove :: [Variable] -> Expression -> Expression
addMove dead (Assignment v e) = Assignment v (addMove dead e)
addMove dead (FunctionCall f@(FuncName "write") e@(Lookup v) args b)
    | v `elem` dead = FunctionCall f e ((Variable "__can_move", ConstBool True):args) b
addMove _ expr = expr

qcInPreprocess :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
qcInPreprocess [] = return []
qcInPreprocess ((lno,expr):rest) = case fastQVar expr of
        Nothing -> ((lno,expr):) <$> qcInPreprocess rest
        Just (fname, v) -> if not $ canQCPreprocessTransform v rest
                    then ((lno,expr):) <$> qcInPreprocess rest
                    else do
                        let expr' = addArgument fname (Variable "__perform_qc", ConstBool False) expr
                            rest' = rewritePreprocess v rest
                        outputListLno' TraceOutput ["Transformation for QC triggered for variable ", show v, " on line ", show lno, "."]
                        ((lno, expr'):) <$> qcInPreprocess rest'

rewritePreprocess _ [] = [] -- this should never happen
rewritePreprocess v ((lno,expr):rest) = case expr of
    FunctionCall f@(FuncName "preprocess") e@(Lookup v') args b
        | v == v' ->
                let expr' = FunctionCall f e ((Variable "__input_qc", ConstBool True):args) b
                    in (lno,expr'):rest
    _ -> (lno,expr):rewritePreprocess v rest

fastQVar :: Expression -> Maybe (T.Text, Variable)
fastQVar (Assignment v (FunctionCall (FuncName fname) _ _ _))
        | fname `elem` ["fastq", "paired"] = Just (fname, v)
fastQVar _ = Nothing

-- The rule is: we can perform the transform if the first usage of the Variable
-- 'v' is in a preproces call. Otherwise, it is not guaranteed to be safe
canQCPreprocessTransform :: Variable -> [(Int, Expression)] -> Bool
canQCPreprocessTransform _ [] = False
canQCPreprocessTransform v ((_,FunctionCall (FuncName "preprocess") (Lookup v') _ _):_)
    | v' == v = True
canQCPreprocessTransform v ((_, expr):rest)
    | isVarUsed1 v expr = False
    | otherwise = canQCPreprocessTransform v rest


ifLenDiscardSpecial :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
ifLenDiscardSpecial = pureTransform $ \case
        (Condition (BinaryOp b (UnaryOp UOpLen (Lookup v)) (ConstInt thresh))
                                    (Sequence [Discard])
                                    (Sequence []))
            | b `elem` [BOpLT, BOpLTE, BOpGT, BOpGTE] -> Optimized (LenThresholdDiscard v b (fromInteger thresh))
        e -> e

substrimReassign :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
substrimReassign = pureTransform $ \case
        (Assignment v (FunctionCall (FuncName "substrim") (Lookup v') [(Variable "min_quality", ConstInt mq)] Nothing))
            | v == v' -> Optimized (SubstrimReassign v (fromInteger mq))
        e -> e
