module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A

import Lava.Vector
import Lava.Bit
import Lava.Word

import Lava.Prelude

exprToAtoms :: (N n) => Expr b -> [Atom n]
exprToAtoms (E.Data value) = [A.Data . boolsToWord $ value]

exprToAtoms (E.Case (E.Data value) cases) =
    [A.Case . boolsToWord $ value] ++
    concatMap (\branch -> Arm (boolsToWord . getData . fst $ branch) :
                         (exprToAtoms . snd $ branch))
              cases
exprToAtoms (E.Case _ _) = error "scrutinee must be a Data expression"

exprToAtoms (E.Let bind bound expr) = undefined -- [A.Let ]

exprToAtomsWithContext :: (N n) => [(b, Word n)] -> Expr b -> [Atom n] -- (Atom ~ Let) in list
exprToAtomsWithContext context (E.Let bind bound expr)  =
    case uniqueWord . map snd $ context of
        Just word -> let newContext = (bind, word) : context in
                         A.Let word :
                         exprToAtomsWithContext newContext bound ++
                         A.In :
                         exprToAtomsWithContext newContext expr ++
                         [A.UnLet word]
        Nothing   -> error "too many bound variables!"