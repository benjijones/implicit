module Implicit.Match where

import Prelude hiding (Word)

import Implicit.AtomType

import Lava.Bit
import Lava.Vector
import Lava.Word
import Lava.Generic

data Pattern =
    A AtomType
  | Any

matchPattern :: (N w) => Pattern -> Word N1 w -> Word N1 N1
matchPattern (A ty) input = fromBit (encodeAtomType ty === input)
matchPattern Any _ = fromBit high

match :: (N w, Generic a) => Vec l Pattern -> Word l w -> a -> a -> a
match patterns input true false = andG (vzipWith matchPattern patterns (separate input)) ? (true, false)