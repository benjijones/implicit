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
      atoms = exprToAtoms letInCase

      program = map A.atomToInteger atoms

      newProc :: New Processor
      newProc = newProcessor program

      results = concatMap (\cycles -> map (\addr -> simRecipe newProc (processor cycles addr) extract) [0..15]) [15]
      extract proc = (val . address $ proc, memory proc, CR.state . caseReducer $ proc)
  mapM_ print atoms
  putStrLn "--------------"
  mapM_ (\(a,b,c) -> putStrLn $ "State: " ++ show (c) ++ " Address: " ++ show a ++ " Memory: " ++ show (A.wordToAtom b)) results

  --mapM_ print results
  --print exampleContext

