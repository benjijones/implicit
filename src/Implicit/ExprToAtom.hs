module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom
import Implicit.AtomType as A
import Implicit.Context

import Lava.Vector

exprToAtoms :: Eq b => Expr b -> [Atom]
exprToAtoms = result . exprToAtomsC

exprToAtomsC :: Eq b => Expr b -> Context b [Atom]
exprToAtomsC (E.Data val) = return [Atom False A.Data val]

exprToAtomsC (E.Case scrut cases) = do
  scrutAtoms <- exprToAtomsC scrut
  caseId <- newCaseBinding
  arms <- sequence . map (caseToArm caseId) $ cases
  return $
    Atom False A.Case caseId :
    scrutAtoms ++
    concat arms ++
    [Atom False A.UnCase caseId]
  where caseToArm caseId (pattern, expr) = do
          expr'  <- exprToAtomsC expr
          pattern' <- exprToAtomsC pattern
          return $ Atom False A.Arm caseId :
                   pattern' ++
                   Atom False A.Arrow caseId :
                   expr'

exprToAtomsC (E.Let bind bound expr) = do
  letBind <- newLetBinding bind
  boundAtoms <- exprToAtomsC bound
  exprAtoms <- exprToAtomsC expr
  return $ Atom False A.Let letBind :
           boundAtoms ++
           Atom False A.In 0 :
           exprAtoms ++
           [Atom False A.UnLet letBind]

exprToAtomsC (E.LetRef bind) = do
    bind' <- getLetBinding bind
    return [Atom False A.LetRef bind']
--exprToAtomsWithContext a b = undefined