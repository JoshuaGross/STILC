module Syntax where

type Name = String

data Expr
  = Var Name
  | Lit Ground
  | App Expr Expr
  | Add Expr Expr
  | Lam Name Type Expr
  | Derive Expr
  deriving (Eq, Show)

data Ground
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Read, Show)
