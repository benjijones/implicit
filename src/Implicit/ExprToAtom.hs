module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A

import Lava.Vector
import Lava.Bit

import Lava.Prelude

exprToAtoms :: Expr b -> [Atom N2]
exprToAtoms (E.Data value) = [A.Data . boolsToWord $ value]

exprToAtoms (E.Case (E.Data value) cases) =
    [A.Case . boolsToWord $ value] ++
    concatMap (\branch -> Arm (boolsToWord . getData . fst $ branch) :
                         (exprToAtoms . snd $ branch))
              cases
exprToAtoms (E.Case _ _) = error "scrutinee must be a Data expression"
   
exprToAtoms (E.Let bind bound expr) = undefined -- [A.Let ]

exprToAtomsWithContext :: Expr b -> [(b, Word n)] -> [Atom N2] -- (Atom ~ Let) in list
exprToAtomsWithContext (E.Let bind bound expr) context =
    let previousNames = map snd context in
    undefined