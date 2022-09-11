module Core where

import Type (Type)

data Expr
  = Var String
  | Lam String Type Expr
  | App Expr Expr
  | String String
  | Int Int
  | IfThenElse Expr Expr Expr
  deriving (Eq, Show)