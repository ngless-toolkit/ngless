{- Copyright 2016 NGLess Authors
 - License: MIT
 -}

module Transform
    ( transform
    ) where

import Language
import NGLess
import Modules


transform :: [Module] -> Script -> NGLessIO Script
transform mods sc = Script (nglHeader sc) <$> applyM transforms (nglBody sc)
    where
        applyM [] e = return e
        applyM (t:ts) e = t e >>= applyM ts
        transforms = modTransforms ++ builtinTransforms
        modTransforms = map modTransform mods
        builtinTransforms =
                [ writeToMove
                ]

writeToMove :: [(Int, Expression)] -> NGLessIO [(Int, Expression)]
writeToMove = return . writeToMove'
writeToMove' [] = []
writeToMove' ((lno,expr):rest) = (lno, addMove toRemove expr):writeToMove' rest
    where
        toRemove = filter (not . flip isVarUsed rest) $ writeVars expr

writeVars :: Expression -> [Variable]
writeVars (ListExpression exprs) = concatMap writeVars exprs
writeVars (UnaryOp _ e) = writeVars e
writeVars (BinaryOp _ e1 e2) = writeVars e1 ++ writeVars e2
writeVars (Assignment _ e) = writeVars e
writeVars (FunctionCall (FuncName "write") (Lookup v) _ _) = [v]
writeVars (Sequence es) = concatMap writeVars es
writeVars _ = []

addMove :: [Variable] -> Expression -> Expression
addMove dead expr = case expr of
    Assignment v e -> Assignment v (addMove dead e)
    FunctionCall fname@(FuncName "write") e@(Lookup v) args b
        | v `elem` dead -> FunctionCall fname e ((Variable "__can_move", ConstBool True):args) b 
    _ -> expr


isVarUsed :: Variable -> [(Int, Expression)] -> Bool
isVarUsed v = any (isVarUsed1 . snd)
    where
        isVarUsed1 :: Expression -> Bool
        isVarUsed1 expr = case expr of
            Lookup v'
                | v' == v -> True
            ListExpression es -> any isVarUsed1 es
            UnaryOp _ e -> isVarUsed1 e
            BinaryOp _ e1 e2 -> isVarUsed1 e1 || isVarUsed1 e2
            Condition ec et ef -> any isVarUsed1 [ec,et,ef]
            IndexExpression e _ -> isVarUsed1 e
            Assignment _ e -> isVarUsed1 e
            FunctionCall _ e args block -> isVarUsed1 e || any (isVarUsed1 . snd) args || maybe False (isVarUsed1 . blockBody) block
            MethodCall _ e me args -> isVarUsed1 e || maybe False isVarUsed1 me || any (isVarUsed1 . snd) args
            _ -> False

