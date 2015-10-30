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


data LetMemory d =
  LetMemory {
    input :: Word d WordN,
    enable :: Bit,

    binding :: Bit,
    unbinding :: Bit,
    select :: Word d DataN,
    offset :: Word d AddressN,
    depth :: Word d AddressN,
    lookupTable :: Word d AddressN,

    output :: Word d WordN
  }


newLetMemory :: Word d WordN -> Bit -> LetMemory d
newLetMemory input enable =

  let -- are we inside a let binding?
      binding = undefined --delay 0  (isLet input) <|> (delay 0 binding <&> (inv (isIn input)))
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
      depth = {-binding ? (delay 0 depth + 1,
              unbinding ? (lookupTable,
              delay 0 depth))-}undefined

      lookupTableInputs = RamInputs {
        ramData = depth,
        ramAddress = unWord select,
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