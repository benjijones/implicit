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
    offset :: Word AddressN
  }


newLetMemory :: Word WordN -> Bit -> LetMemory AddressN WordN
newLetMemory input enable =

  let -- are we inside a let binding?
      binding = delay 0  (isLet input) <|> (delay 0 binding <&> (inv (isIn input)))
      -- are we unbinding?
      unbinding = isUnLet input
      -- which Let is currently selected in the lookup table
      select = (isLet input <|> isUnLet input) ? (contentBits input, delay 0 select)
      -- how far into a single Let
      offset = binding ? (delay 0 offset + 1, 0)
      -- pointer to next unused word in memory
      -- TODO: decrease when unbinding Let (UnLet)
      depth = binding ? (delay 0 depth + 1,
              unbinding ? (lookupTable,
              delay 0 depth))

      lookupTableInputs = RamInputs {
        ramData = depth,
        ramAddress = select,
        ramWrite = enable <&> isLet input
      }
      lookupTable = ram [] Width9 lookupTableInputs
      letMemoryInputs = RamInputs {
        ramData = input,
        ramAddress = lookupTable + offset,
        ramWrite = enable <&> binding
      }
      output = ram [] Width9 letMemoryInputs in

  LetMemory {
    input,
    enable,
    output,
    offset
  }