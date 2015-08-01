module Main where

import Implicit.Expr
import qualified Implicit.Atom as A
import Implicit.ExprToAtom

import Lava.Vector

main :: IO ()
main =
  let expr = Let "x" (Data [True,False]) (LetRef "x")
      result :: [A.Atom N2]
      result = exprToAtoms expr in
  putStr . unlines . map show $ result