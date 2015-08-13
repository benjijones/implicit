module Implicit.ExprToAtom where

import Implicit.Expr as E
import Implicit.Atom as A
import Implicit.Context

import Lava.Vector

exprToAtoms :: (N n, Eq b) => Expr b -> [Atom n]
exprToAtoms = result . exprToAtomsC

exprToAtomsC :: (N n, Eq b) => Expr b -> Context b [Atom n]
exprToAtomsC (E.Data val) = return [A.Data val False]

exprToAtomsC (E.Case scrut cases) = do
  scrutAtoms <- exprToAtomsC scrut
  caseId <- newCaseBinding
  arms <- sequence . map (caseToArm caseId) $ cases
  return $
    A.Case caseId False :
    scrutAtoms ++
    concat arms ++
    [A.UnCase caseId False]
  where caseToArm caseId (pattern, expr) = do
          expr'  <- exprToAtomsC expr
          pattern' <- exprToAtomsC pattern
          return $ Arm caseId False :
                   pattern' ++
                   Arrow False :
                   expr'

exprToAtomsC (E.Let bind bound expr) = do
  letBind <- newLetBinding bind
  boundAtoms <- exprToAtomsC bound
  exprAtoms <- exprToAtomsC expr
  return $ A.Let letBind False :
           boundAtoms ++
           A.In False :
           exprAtoms ++
           [A.UnLet letBind False]

exprToAtomsC (E.LetRef bind) = do
    bind' <- getLetBinding bind
    return [A.LetRef bind' False]
--exprToAtomsWithContext a b = undefined