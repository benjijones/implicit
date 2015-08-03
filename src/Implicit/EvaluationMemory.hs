module Implicit.EvaluationMemory where

import Lava.Word
import Lava.Recipe
import Lava.Vector
import Lava.Prelude

data EvaluationMemory m n =
  EvaluationMemory {
    address :: Sig m,
    output :: Word n
  }

newEvaluationMemory :: (N n, N m) => New (EvaluationMemory m n)
newEvaluationMemory = do
  addr <- newSig

  let ramInput = RamInputs {
        
      }

      ramOutput = ram 

  return $ EvaluationMemory {
    address = addr,
    output = ramOutput
  }