module Main where

import Implicit.Expr
import qualified Implicit.Atom as A
import Implicit.ExprToAtom
import Implicit.LetReplacer

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe

main :: IO ()
main = do
    let newLR :: New (LetReplacer N3)
        newLR = newLetReplacer
        result = simRecipe newLR letReplace reference in
      print result