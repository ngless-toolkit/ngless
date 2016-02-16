{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module Transform
    ( transform
    ) where

import qualified Data.Text as T

import Language
import Modules
import Output
import Utils.Debug
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
                ]


addArgument :: T.Text -> (Variable, Expression) -> Expression -> Expression
addArgument func newArg expr = case expr of
    Assignment v e -> Assignment v (addArgument func newArg e)
    FunctionCall fname@(FuncName fname') e args b
        | fname' == func ->
            FunctionCall fname e (newArg:args) b
    _ -> expr

isVarUsed :: Variable -> [(Int, Expression)] -> Bool
isVarUsed v = any (isVarUsed1 v . snd)

isVarUsed1 :: Variable -> Expression -> Bool
isVarUsed1 v expr = case expr of
    Lookup v'
        | v' == v -> True
    ListExpression es -> any (isVarUsed1 v) es
    UnaryOp _ e -> isVarUsed1 v e
    BinaryOp _ e1 e2 -> isVarUsed1 v e1 || isVarUsed1 v e2
    Condition ec et ef -> any (isVarUsed1 v) [ec,et,ef]
    IndexExpression e _ -> isVarUsed1 v e
    Assignment _ e -> isVarUsed1 v e
    FunctionCall _ e args block -> isVarUsed1 v e || any (isVarUsed1 v . snd) args || maybe False (isVarUsed1 v . blockBody) block
    MethodCall _ e me args -> isVarUsed1 v e || maybe False (isVarUsed1 v) me || any (isVarUsed1 v . snd) args
    _ -> False

writeToMove :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
writeToMove = return . writeToMove'
writeToMove' [] = []
writeToMove' ((lno,expr):rest) = (lno, addMove toRemove expr):writeToMove' rest
    where
        toRemove = filter (not . flip isVarUsed rest) $ functionVars "write" expr

-- | Variables used in calling the function func
functionVars :: T.Text -> Expression -> [Variable]
functionVars func (ListExpression exprs) = concatMap (functionVars func) exprs
functionVars func (UnaryOp _ e) = functionVars func e
functionVars func (BinaryOp _ e1 e2) = functionVars func e1 ++ functionVars func e2
functionVars func (Assignment _ e) = functionVars func e
functionVars func (FunctionCall (FuncName func') (Lookup v) _ _)
    | func' == func = [v]
functionVars func (Sequence es) = concatMap (functionVars func) es
functionVars _ _ = []

addMove :: [Variable] -> Expression -> Expression
addMove dead (Assignment v e) = Assignment v (addMove dead e)
addMove dead (FunctionCall f@(FuncName "write") e@(Lookup v) args b)
    | v `elem` dead = FunctionCall f e ((Variable "__can_move", ConstBool True):args) b
addMove _ expr = expr

qcInPreprocess :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
qcInPreprocess [] = return []
qcInPreprocess ((lno,expr):rest) = case fastQVar expr of
        Nothing -> ((lno,expr):) <$> qcInPreprocess rest
        Just v -> if not $ canQCPreprocessTransform v rest
                    then ((lno,expr):) <$> qcInPreprocess rest
                    else do
                        let expr' = addArgument "fastq" (Variable "__perform_qc", ConstBool False) expr
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

fastQVar :: Expression -> Maybe Variable
fastQVar (Assignment v (FunctionCall (FuncName "fastq") _ _ _)) = Just v
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

