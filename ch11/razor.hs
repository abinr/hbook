module Razor where

data Expr
  = Lit Integer
  | Add Expr Expr
  deriving Show

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add a b) = show a ++ "+" ++ show b

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b

