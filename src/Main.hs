module Main where

import qualified Implicit.Expr as E
import Implicit.ExprToAtom
import qualified Implicit.Atom as A
import Implicit.LetReplacer
import Implicit.EvaluationMemory
import Implicit.Processor

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
  let exampleProgram = E.Let "x" (E.Data 1) (E.LetRef "x")
      atoms :: [A.Atom N5]
      atoms = exprToAtoms exampleProgram

      program = map A.atomToInteger atoms

      newProc :: New Processor
      newProc = newProcessor program

      result = simRecipe newProc processor (memory . letReplacer)
  mapM_ print program
  print result