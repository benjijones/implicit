module Implicit.Expr where

data Expr =
    Data [Bool]
  | Case Expr [(Expr, Expr)] -- (Data, Expr)
  | Let Expr Expr
  | LetRef Int
  deriving (Show, Read)

getData :: Expr -> [Bool]
getData (Data a) = a
getData _        = error "getData failed"

