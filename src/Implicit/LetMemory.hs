{-# LANGUAGE NamedFieldPuns #-}
module Implicit.LetMemory where

import Implicit.BitWidths
import Implicit.Atom

import Lava.Recipe
import Lava.Word
import Lava.Vector
import Lava.Ram


data LetMemory m n =
  LetMemory {
    output :: Word WordN,
    offset :: Reg N4
  }

newLetMemory :: Word WordN -> New (LetMemory AddressN WordN)
newLetMemory input = do

  offset <- newReg
  

  let lookupTableInputs = RamInputs {
        ramData = offset!val,
        ramAddress = contentBits input,
        ramWrite = isLet input
      }
      lookupTable = ram [] Width9 lookupTableInputs
  return $ LetMemory {
    offset
  }