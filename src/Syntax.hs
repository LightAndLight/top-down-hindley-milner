module Syntax where

import Type (Scheme)

data Span = Span {pos :: !Int, len :: !Int}
  deriving (Eq, Show)

data Spanned a = Spanned {span :: !Span, value :: a}
  deriving (Eq, Show)

data Expr
  = Var String
  | Lam String (Spanned Expr)
  | App (Spanned Expr) (Spanned Expr)
  | With String Scheme (Spanned Expr)
  | String String
  | Int Int
  | IfThenElse (Spanned Expr) (Spanned Expr) (Spanned Expr)
  deriving (Eq, Show)