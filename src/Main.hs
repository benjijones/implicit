module Main where

import qualified Implicit.Expr as E
import Implicit.ExprToAtom
import qualified Implicit.Atom as A
import Implicit.LetReplacer as LR
import Implicit.CaseReducer as CR
import Implicit.EvaluationMemory
import Implicit.Examples
import Implicit.Processor
import Implicit.LetMemory

import Lava.Vector
import Lava.Binary
import Lava.Bit
import Lava.Recipe
import Lava.Binary
import Lava.Vhdl
import Lava.Ram
import Lava.Word
import Lava.Generic

main :: IO ()
main = do
  {-let extract proc = (val . address $ proc, memory proc, CR.state . caseReducer $ proc)
     results = simulateProcessor [15] [0..15] extract letInCase

  mapM_ (print) (exprToAtoms letInCase :: [A.Atom N5])
  putStrLn "--------------"
  mapM_ (\(a,b,c) -> putStrLn $ "State: " ++ show (c) ++ " Address: " ++ show a ++ " Memory: " ++ show (A.wordToAtom b)) results
  -}
  let letMemoryInput = delay (fromInteger . A.atomToInteger $ A.Let 0 False)
                             (fromInteger . A.atomToInteger $ A.LetRef 0 False)
  mapM_ print . simulateN 5 . offset $ newLetMemory letMemoryInput 1

