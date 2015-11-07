module Implicit.EvaluationMemory where

import Prelude hiding (Word)

import Lava.Word
import Lava.Vector
import Lava.Ram
{-
type EvaluationMemory d addrW dataW = Word d dataW


evaluationMemory :: (N dataW, N addrW) => RamInputs dataW addrW -> [Integer] -> EvaluationMemory d addrW dataW
evaluationMemory ramInputs contents = Word $ ram contents Width9 ramInputs
-}