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
  let letMemoryInput = A.atomsToWord
                  [ A.Let 1 False
                  , A.Data 1 False
                  , A.Data 1 False
                  , A.Data 1 False
                  , A.In False
                  , A.Let 2 False
                  , A.Data 2 False
                  , A.In False
                  , A.LetRef 1 False
                  , A.LetRefPadding False
                  , A.LetRef 2 False
                  , A.UnLet 2 False
                  , A.UnLet 1 False]
  mapM_ print . simulateN 11 . (depth) $ newLetMemory letMemoryInput 1

