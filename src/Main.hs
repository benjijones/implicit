module Main where

import qualified Implicit.Expr as E
import Implicit.ExprToAtom
import qualified Implicit.Atom as A
import Implicit.LetReplacer
import Implicit.EvaluationMemory
import Implicit.Processor
import Implicit.Examples

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe
import Lava.Binary
import Lava.Vhdl
import Lava.Ram
import Lava.Word

main :: IO ()
main = do
  let atoms :: [A.Atom N5]
      atoms = exprToAtoms exampleCase

      program = map A.atomToInteger atoms

      newProc :: New Processor
      newProc = newProcessor program

      result = simRecipe newProc (processor 0) (memory . letReplacer)
  mapM_ print atoms
  print (A.wordToAtom result)
  --print exampleContext

