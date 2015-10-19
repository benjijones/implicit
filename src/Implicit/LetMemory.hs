{-# LANGUAGE NamedFieldPuns #-}
module Implicit.LetMemory where

import Prelude hiding (Word)

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

    binding :: Bit,
    unbinding :: Bit,
    select :: Word DataN,
    offset :: Word AddressN,
    depth :: Word AddressN,
    lookupTable :: Word AddressN,

    output :: Word WordN
  }


newLetMemory :: Word WordN -> Bit -> LetMemory AddressN WordN
newLetMemory input enable =

  let -- are we inside a let binding?
      binding = delay 0 (isLet input) <|> (delay 0 binding <&> (inv (isIn input)))
      -- are we unbinding?
      unbinding = isUnLet input
      -- are we inside a let reference?
      letref = isLetRef input <|> isLetRefPadding input
      -- which Let is currently selected in the lookup table
      select = (isLet input <|> isUnLet input <|> (isLetRef input <&> inv binding)) ? (contentBits input, delay 0 select)
      -- how far into a particular Let or LetRef
      offset = (binding <&> delay 0 binding) ? (delay 0 offset + 1,
                 isLetRefPadding input ? (delay 0 offset + 1, 0))
      -- pointer to next unused word in memory
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
    binding,
    unbinding,
    select,
    offset,
    depth,
    lookupTable,
    output
  }