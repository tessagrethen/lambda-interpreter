{- Author: Tessa Pham
   File: Eval.hs
   Description: Defines an evaluator for Preλ.
-}

module Eval where

import Syntax

-- Evaluate an expression to a value. Call `error` if this is impossible.
eval :: Expr -> Value
eval (IfE ex1 ex2 ex3)
  | BoolV boolEx1 <- eval ex1, boolEx1 == True = eval ex2
  | BoolV boolEx1 <- eval ex1, boolEx1 == False = eval ex3
  | otherwise = error "invalid expression"
eval (OpE op ex1 ex2)
  | IntegerV n1 <- eval ex1, IntegerV n2 <- eval ex2 = performOp op n1 n2
  | otherwise = error "invalid expression"
eval (NotE ex)
  | BoolV boolEx <- eval ex = BoolV (not boolEx)
  | otherwise = error "invalid expression"
eval (ValueE n) = n
eval (VarE n) = error "strings can't be evaluated"
eval (AppE (ValueE (LambdaV n1 ex1)) ex)
  | (ValueE (LambdaV n2 ex2)) <- ex1, n2 /= n1 = eval (ValueE (LambdaV n2 (subst ex2 n1 (eval ex))))
  | otherwise =  eval (subst ex1 n1 (eval ex))
eval (AppE ex1 ex2) = eval (AppE (ValueE (eval ex1)) ex2)

-- All binary operators take two Integer arguments. This function
-- performs the operation on the arguments, returning a Value.
performOp :: Op -> Integer -> Integer -> Value
performOp Plus x y = IntegerV (x + y)
performOp Minus x y = IntegerV (x - y)
performOp Times x y = IntegerV (x * y)
performOp Divides x y = IntegerV (x `div` y)
performOp LessThan x y = BoolV (x < y)
performOp LessThanEquals x y = BoolV (x <= y)
performOp GreaterThan x y = BoolV (x > y)
performOp Equals x y = BoolV (x == y)
performOp NotEquals x y = BoolV (x /= y)

-- Substitute a value into an expression.
subst :: Expr -> String -> Value -> Expr
subst ex s val
  | (VarE n) <- ex, n <- s = ValueE val
  | (AppE (VarE n1) (VarE n2)) <- ex, n1 == n2, n1 <- s, n2 <- s
  = AppE (ValueE val) (ValueE val)
  | (AppE (VarE n) ex1) <- ex, n <- s = AppE (ValueE val) ex1
  | (AppE ex1 (VarE n)) <- ex, n <- s = AppE ex1 (ValueE val)
  | (OpE op (VarE n1) (VarE n2)) <- ex, n1 == n2, n1 <- s, n2 <- s
  = OpE op (ValueE val) (ValueE val)
  | (OpE op (VarE n) ex1) <- ex, n <- s = OpE op (ValueE val) ex1
  | (OpE op ex1 (VarE n)) <- ex, n <- s = OpE op ex1 (ValueE val)
  | otherwise = ex
