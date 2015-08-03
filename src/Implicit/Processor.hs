{-# LANGUAGE NamedFieldPuns #-}
module Implicit.Processor where

import Implicit.EvaluationMemory
import Implicit.LetReplacer
import Implicit.CaseReducer

import Lava.Bit
import Lava.Vector
import Lava.Recipe

data Processor =
  Processor {
    memory :: EvaluationMemory N11 N9,
    letReplacer :: LetReplacer N11 N5,
    finished :: Sig N1
  }

newProcessor :: New (Processor)
newProcessor = do
  memory <- newEvaluationMemory
  letReplacer <- newLetReplacer memory

  finished <- newSig

  return $ Processor {
    memory,
    letReplacer,
    finished
  }

processor :: Processor -> Recipe
processor p =
  While (p!finished!val!vhead!inv) $
  Seq [
    p!letReplacer!letReplace
  , p!finished <== 1
  ]