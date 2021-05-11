{-# LANGUAGE GADTs #-}

module Expr where

data Expr a where
  LInt :: Int -> Expr Int
  LBool :: Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LInt i) = i
evalExpr (LBool b) = b
evalExpr (Not b) = not $ evalExpr b
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (If c a b) = if evalExpr c then evalExpr a else evalExpr b
