module Implicit.Adder where

import Prelude hiding (Word, splitAt)

import Implicit.AtomType

import Lava.Word
import Lava.Vector
import Lava.Generic
             
adder :: (N w) => (Word N3 w) -> (Word N3 w)
adder input = match (encode $ vmap (encode) $ Add +> Data +> Data +> vempty) (fst $ splitAt n4 $ input)
                    (encode $ (unWord input `vat` n1) + (unWord input `vat` n2) +> vrepeat 0)
                    input