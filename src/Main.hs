module Main where

import qualified Implicit.Expr as E
import Implicit.ExprToAtom
import qualified Implicit.AtomType as A
import Implicit.Atom
--import Implicit.LetReplacer as LR
--import Implicit.CaseReducer as CR
import Implicit.EvaluationMemory
--import Implicit.Examples
--import Implicit.LetMemory

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

  let atoms = [ Atom False A.Let 1
                , Atom False A.Data 1
                , Atom False A.Data 1
              , Atom False A.In 0
                , Atom False A.Let 2
                  , Atom False A.Data 2
                , Atom False A.In 0
                  , Atom False A.LetRef 1
                  , Atom False A.LetRefPadding 0
                  , Atom False A.LetRefPadding 0
                  , Atom False A.LetRef 0
                , Atom False A.UnLet 2
              , Atom False A.UnLet 1]
  mapM_ print . zip [1..] $ atoms
--  mapM_ print . zip [1..] . simulateN 12 . (\a -> (select a, output a)) $ newLetMemory letMemoryInput 1
