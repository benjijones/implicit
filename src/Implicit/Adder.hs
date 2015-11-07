module Implicit.Adder where

import Prelude hiding (Word)

import Implicit.Atom
import Implicit.Sentence

import Lava.Word
import Lava.Vector

validInput :: Word l N4
validInput = vmap fromInteger (typeEncoding (Add False) +> typeEncoding (Data 1 False) +> typeEncoding (Data 0 False) +> vempty)

adder :: (N w) => (Word (S (S (S Z))) w) -> (Word (S Z) w)
adder input = (input `vat` n1) + (input `vat` n2) +> vempty