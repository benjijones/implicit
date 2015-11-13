module Implicit.Adder where

import Prelude hiding (Word, splitAt)

import Implicit.AtomType
import Implicit.Atom
import Implicit.Match

import Lava.Word
import Lava.Vector
             
adder :: Word N3 AtomN -> Word N3 AtomN
adder input = match (A Add +> A Data +> A Data +> vempty)
                    (fst $ splitAt n4 $ input)
                    (Word $ (unWord input `vat` n1) + (unWord input `vat` n2) +> vrepeat 0)
                    input