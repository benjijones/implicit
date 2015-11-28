{-# LANGUAGE GADTs #-}

module Implicit.Match where

import Prelude hiding (Word)

import Implicit.Atom
import Implicit.AtomType
import Implicit.Word
import Implicit.PolyTree

import Lava.Bit
import Lava.Vector
import Lava.Generic

data Pattern where
  A     :: AtomType -> Pattern
  Any   :: Pattern


matchPattern :: Pattern -> Word AtomTypeN -> Word N1
matchPattern (A ty) input = fromBit (encodeAtomType ty === input)
matchPattern Any _ = 1

match :: PolyTree Pattern -> Word AtomN -> a -> a -> a
match patterns input true false = andG (vzipWith matchPattern
                                        patterns
                                        (separate $ typeBits input))
                                   ? (true, false)