{-# LANGUAGE NamedFieldPuns #-}
module Implicit.LetMemory where

import Implicit.BitWidths
import Implicit.Atom

import Lava.Bit
import Lava.Recipe
import Lava.Word
import Lava.Vector
import Lava.Ram
import Lava.Generic


data LetMemory m n =
  LetMemory {
    input :: Word WordN,
    enable :: Bit,
    output :: Word WordN,
    offset :: Word N4
  }

newLetMemory :: Word WordN -> Bit -> LetMemory AddressN WordN
newLetMemory input enable =

  let update = (isLet input) <&> enable
      offset = delay (0 :: Word N4) (update ? (offset + 1, 0))

      lookupTableInputs = RamInputs {
        ramData = offset,
        ramAddress = contentBits input,
        ramWrite = isLet input <&> enable
      }
      lookupTable = ram [1] Width9 lookupTableInputs
      letMemoryInputs = RamInputs {
        ramData = input,
        ramAddress = lookupTable + offset,
        ramWrite = isLet input <&> enable
      }
      output = ram [0, 1] Width9 letMemoryInputs in

  LetMemory {
    input,
    enable,
    output,
    offset
  }