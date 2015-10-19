{-# LANGUAGE FlexibleInstances #-}

module Lava.Word where

import Prelude hiding (Word)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Arithmetic
import Lava.Generic

import Data.List(find)

-- | Notably, an instance of the Num class.
type Word n = Vec n Bit

-- | Unsigned bit-vectors.
type Unsigned n = Word n

-- | Convert a bit-vector to a list of its integer outputs
wordToInts :: Integral a => Word n -> [a]
wordToInts = map binToNat . map bitToBools . velems

-- | Convert bit-vector to an integer.
wordToInt :: Integral a => Word n -> a
wordToInt = binToNat . map bitToBool . velems

instance Eq (Vec n Bit) where
  a == b = error msg
    where msg = "== and /= on bit-vectors is not supported: try === and =/="

instance N n => Num (Vec n Bit) where
  a + b = vec (velems a /+/ velems b)
  a - b = vec (velems a /-/ velems b)
  a * b = error "Multiplication of bit-vectors is not yet supported"
  abs a = a
  -- just 0 or 1 as vectors are interpreted as unsigned
  signum v = vec (orG xs : repeat 0)
    where xs = velems v
  fromInteger i = sized (\n -> Vec (i `ofWidth` n))

ofWidth :: Integral a => a -> Int -> [Bit]
n `ofWidth` s = map boolToBit (intToSizedBin n s)