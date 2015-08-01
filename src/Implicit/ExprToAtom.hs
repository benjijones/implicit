module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A

import Lava.Vector
import Lava.Bit
import Lava.Word
import Lava.Binary

import Lava.Prelude

import Data.List (find)

exprToAtoms :: (N n, Eq b) => Expr b -> [Atom n]
exprToAtoms = exprToAtomsWithContext []

exprToAtomsWithContext :: (N n, Eq b) => [(b, Integer)] -> Expr b -> [Atom n]
exprToAtomsWithContext _ (E.Data value) = [A.Data value]

exprToAtomsWithContext _ (E.Case (E.Data value) cases) =
    [A.Case value] ++
    concatMap (\branch -> Arm (getData . fst $ branch) :
                         (exprToAtoms . snd $ branch))
              cases

exprToAtomsWithContext _ (E.Case _ _) = error "scrutinee must be a Data expression"

exprToAtomsWithContext context (E.Let bind bound expr) =
    case {-uniqueWord . map snd $ context-} Just 1 of
        Just word -> let newContext = (bind, word) : context in
                         A.Let word :
                         exprToAtomsWithContext newContext bound ++
                         A.In :
                         exprToAtomsWithContext newContext expr ++
                         [A.UnLet word]
        Nothing   -> error "too many bound variables!"

exprToAtomsWithContext context (E.LetRef bind) =
    case find ((== bind) . fst) context of
        Just (_, w) -> [A.LetRef w]
        Nothing -> error "LetRef to unknown reference"