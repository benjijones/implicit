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
             Let "y" (Data 1) $
             LetRef "x"
      result :: [A.Atom N3]
      result = exprToAtoms expr in
      do
      mapM_ print result
      mapM_ (print . A.atomToVec n3) result