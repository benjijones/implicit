module Implicit.Match where

import Prelude hiding (Word)

import Implicit.Atom
import Implicit.AtomType

import Lava.Bit
import Lava.Vector
import Lava.Word
import Lava.Generic

data Pattern =
    A AtomType
  | Any

matchPattern :: Pattern -> Word N1 AtomTypeN -> Word N1 N1
matchPattern (A ty) input = fromBit (encodeAtomType ty === input)
matchPattern Any _ = fromBit high

match :: Generic a => Vec l Pattern -> Word l AtomN -> a -> a -> a
match patterns input true false = andG (vzipWith matchPattern
                                        patterns
                                        (separate $ typeBits input))
                                   ? (true, false)