module Implicit.EvaluationMemory where

import Lava.Word
import Lava.Vector
import Lava.Ram

type EvaluationMemory m n = Word n


evaluationMemory :: (N m, N n) => RamInputs n m -> [Integer] -> EvaluationMemory m n
evaluationMemory ramInputs contents = ram contents Width9 ramInputs