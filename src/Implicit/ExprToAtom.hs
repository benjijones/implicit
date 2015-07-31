module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A

import Lava.Vector
import Lava.Bit

exprToAtoms :: Expr -> [Atom (S (S Z))]
exprToAtoms (E.Data value) = [A.Data . boolsToVec $ value]

exprToAtoms (E.Case (E.Data value) cases) =
      [A.Case . boolsToVec $ value] ++
      concatMap (\branch -> Arm (boolsToVec . getData . fst $ branch) :
                           (exprToAtoms . snd $ branch))
                cases
exprToAtoms (E.Case _ _) = error "scrutinee must be a Data expression"
   
exprToAtoms (E.Let bind expr) = u[A.Let ]

boolsToVec :: (N n) => [Bool] -> Vec n Bit
boolsToVec = vec . map boolToBit