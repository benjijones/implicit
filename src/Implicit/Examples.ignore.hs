module Implicit.Examples where

import Prelude hiding (Word)

import qualified Implicit.Atom as A
import Implicit.ExprToAtom
import Implicit.Expr
import Implicit.Context
import Implicit.Processor

import Lava.Vector
import Lava.Recipe
import Lava.Bit
import Lava.Word
import Lava.Ram
import Lava.Prelude


simulateProcessor :: (Generic a) => [Integer] -> [Integer] -> (Processor -> a) -> Expr String -> [a]
simulateProcessor cycles address extract expr = let
  atoms :: [A.Atom N5]
  atoms = exprToAtoms expr

  program = map A.atomToInteger atoms

  newProc :: New Processor
  newProc = newProcessor program in

  [simRecipe newProc (processor cyc addr) extract | cyc <- cycles, addr <- address]

basicLet :: Expr String
basicLet = Let "x" (Data 1) (LetRef "x")

basicCase :: Expr String
basicCase = Case (Data 1)
                [ (Data 0, Data 2)
                , (Data 1, Data 2)
                ]

caseInLet :: Expr String
caseInLet = Let "x" (Data 1) $
                     Case (Data 0)
                       [ (Data 0, LetRef "x")
                       , (Data 1, Data 2)
                       ]

letInCase :: Expr String
letInCase = Case (Data 1)
              [ (Data 0, Data 1)
              , (Data 1, Let "x" (Data 3) (LetRef "x"))
              ]

exampleContext :: Context String Integer
exampleContext = do
    x <- newLetBinding "x"
    y <- newLetBinding "y"
    z <- newCaseBinding
    z1 <- newCaseBinding
    w <- newLetBinding "x"
    x1 <- getLetBinding "x"
    return (x1)

simpleRAM :: Word N9
simpleRAM = ram [] Width9 $ RamInputs {
    ramData = 1 :: Word N9,
    ramAddress = delay 1 . delay 1 $ 0 :: Word N11,
    ramWrite = 1
  }


atomProgram = Atom False A.Let 1
              +> Atom False A.Data 1
              +> Atom False A.Data 1
            +> Atom False A.In 0
              +> Atom False A.Let 2
                +> Atom False A.Data 2
              +> Atom False A.In 0
                +> Atom False A.LetRef 1
                +> Atom False A.LetRefPadding 0
                +> Atom False A.LetRefPadding 0
                +> Atom False A.LetRef 0
              +> Atom False A.UnLet 2
            +> Atom False A.UnLet 1
            +> vempty