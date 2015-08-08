module Implicit.EvaluationMemory where

import Lava.Word
import Lava.Recipe
import Lava.Vector
import Lava.Ram
import Lava.Bit

import Debug.Trace

type EvaluationMemory n = Word n


evaluationMemory :: (N m, N n) => RamInputs n m -> [Integer] -> EvaluationMemory n
evaluationMemory ramInputs contents = ram contents Width9 ramInputs