module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A

import Lava.Vector

import Data.List (find)

exprToAtoms :: (N n, Eq b) => Expr b -> [Atom n]
exprToAtoms = exprToAtomsWithContext []

exprToAtomsWithContext :: (N n, Eq b) => [(b, Integer)] -> Expr b -> [Atom n]
exprToAtomsWithContext _ (E.Data val) = [A.Data val False]

exprToAtomsWithContext c (E.Case scrut cases) =
    A.Case False :
    exprToAtomsWithContext c scrut ++
    concatMap (\(pattern, expr) -> Arm (getData pattern) False :
                         exprToAtomsWithContext c expr)
              cases

exprToAtomsWithContext context (E.Let bind bound expr) =
    let uniqueId = unique . map snd $ context
        newContext = (bind, uniqueId) : context in
                         A.Let uniqueId False :
                         exprToAtomsWithContext newContext bound ++
                         A.In False :
                         exprToAtomsWithContext newContext expr ++
                         [A.UnLet uniqueId False]
    where unique prevIds = if null prevIds then 0 else head prevIds + 1 -- NAIVE: FIX WHEN YOU FEEL UP TO IT

exprToAtomsWithContext context (E.LetRef bind) =
    case find ((== bind) . fst) context of
        Just (_, w) -> [A.LetRef w False]
        Nothing -> error "LetRef to unknown reference"