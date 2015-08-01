module Main where

import Implicit.Expr
import qualified Implicit.Atom as A
import Implicit.ExprToAtom

import Lava.Vector
import Lava.Binary
import Lava.Bit

main :: IO ()
main =
  let
      expr = Let "x" (Data 0) $
             Case (LetRef "x") [
             (Data 0, Data 2) ,
             (Data 1, Data 3)
             ]
      result :: [A.Atom N4]
      result = exprToAtoms expr in
      do
      mapM_ print result
      mapM_ (print . A.atomToVec) result