{-# LANGUAGE NamedFieldPuns #-}
module Implicit.EvaluationMemory where

import Lava.Word
import Lava.Recipe
import Lava.Vector
import Lava.Ram

data EvaluationMemory m n =
  EvaluationMemory {
    address :: Sig m,
    input :: Sig n,
    write :: Sig N1,
    output :: Word n
  }

newEvaluationMemory :: (N n, N m) => New (EvaluationMemory m n)
newEvaluationMemory = do
  address <- newSig
  input <- newSig
  write <- newSig

  let ramInput = RamInputs {
        ramData = input!val,
        ramAddress = address!val,
        ramWrite = write!val!vhead
      }

      ramOutput = ram [] Width9 ramInput

  return $ EvaluationMemory {
    address,
    input,
    write,
    output = ramOutput
  }