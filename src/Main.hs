module Main where

import qualified Implicit.Expr as E
import Implicit.ExprToAtom
import qualified Implicit.Atom as A
import Implicit.LetReplacer as LR
import Implicit.CaseReducer as CR
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

      results = map (\x -> simRecipe newProc (processor x) extract) [0..15]
      extract proc = (val . address $ proc, memory proc)
  mapM_ print atoms
  putStrLn "--------------"
  mapM_ (\(a,b) -> putStrLn $ show (a) ++ " ---> " ++ show (A.wordToAtom b)) results

  --mapM_ print results
  --print exampleContext

