{-# LANGUAGE NamedFieldPuns #-}
module Implicit.Processor where

import Implicit.EvaluationMemory
import Implicit.LetReplacer
import Implicit.CaseReducer

import Lava.Bit
import Lava.Vector
import Lava.Recipe

import Debug.Trace

data Processor =
  Processor {
    --memory :: EvaluationMemory N11 N9,
    letReplacer :: LetReplacer N11 N5,
    finished :: Sig N1
  }

newProcessor :: [Integer] -> New Processor
newProcessor program = do
  letReplacer <- newLetReplacer program

  finished <- newSig

  return $ Processor {
    letReplacer,
    finished
  }

processor :: Processor -> Recipe
processor p =
  While (p!finished!val!vhead!inv) $
  Seq [
    p!finished <== 1
  , Tick
  ]