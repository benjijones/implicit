module Implicit.Expr where

data Expr b =
    Data Integer
  | Case (Expr b) [(Expr b, Expr b)] -- (Data, Expr)
  | Let b (Expr b) (Expr b)
  | LetRef b
  deriving (Show, Read)

getData :: Expr b -> Integer
getData (Data a) = a
getData _        = error "getData called on non-Data Expr"

