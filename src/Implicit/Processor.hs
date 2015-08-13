{-# LANGUAGE NamedFieldPuns #-}
module Implicit.Processor where

import Implicit.EvaluationMemory
import Implicit.LetReplacer as LR
import Implicit.CaseReducer as CR

import Lava.Bit
import Lava.Vector
import Lava.Recipe
import Lava.Generic

data Processor =
  Processor {
    --memory :: EvaluationMemory N11 N9,
    letReplacer :: LetReplacer N11 N5,
    caseReducer :: CaseReducer N11 N5,
    cycles :: Reg N4
    --finished :: Sig N1
  }

newProcessor :: [Integer] -> New Processor
newProcessor program = do
  letReplacer <- newLetReplacer program
  caseReducer <- newCaseReducer program
  cycles <- newReg
  --finished <- newSig

  return $ Processor {
    letReplacer,
    caseReducer,
    cycles
    --finished
  }

processor :: Integer -> Processor -> Recipe
processor finalAddress p =
  Seq [
    Tick
  , While (p!letReplacer!LR.state!val =/= 3 <|> p!letReplacer!LR.address!val =/= fromInteger finalAddress) $
    Seq [
      p!letReplacer!letReplace
    , p!caseReducer!caseReduce
    , p!cycles <== p!cycles!val + 1
    , Tick
    ]
  ]