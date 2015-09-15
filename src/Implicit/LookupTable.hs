{-# LANGUAGE NamedFieldPuns #-}
module Implicit.LookupTable where

import Lava.Vector
import Lava.Word
import Lava.Recipe

data LookupTable =
  LookupTable {
    entries :: Vec N8 (Word N11),
    address :: Word N3
  }

newLookupTable :: Word N11 -> New LookupTable
newLookupTable address = do
  regs <- sequence $ repeat $ newReg

  return $ LookupTable {
    entries = vec $ map val regs,
    address = vtake n3 address
  }