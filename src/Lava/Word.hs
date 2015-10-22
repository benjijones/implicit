{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Lava.Word where

import Prelude hiding (Word)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Arithmetic
import Lava.Generic

import Data.List(find)

-- Contains a Bit signal with
-- type-level encodings of:
--  the # of cycle delays in the signal
--  the width of the signal
newtype Word delay width = Word { unWord :: Vec width Bit }

getBits :: Word d w -> [Bit]
getBits (Word vec) = velems vec

-- | Convert a Word to a list of its integer outputs
wordToInts :: Integral a => Word d w -> [a]
wordToInts = map binToNat . map bitToBools . getBits

-- | Convert bit-vector to an integer.
-- uses 'head' but should be safe unless a 
wordToInt :: Integral a => Word d w -> a
wordToInt = head . wordToInts

instance Show (Word d n) where
  show = show . unWord

instance Eq (Vec n Bit) where
  a == b = error msg
    where msg = "== and /= on bit-vectors is not supported: try === and =/="

instance N n => Num (Word d n) where
  (Word a) + (Word b) = Word $ vec (velems a /+/ velems b)
  (Word a) - (Word b) = Word $ vec (velems a /-/ velems b)
  (Word a) * (Word b) = error "Multiplication of Words is not yet supported"
  abs (Word a) = Word $ a
  -- just 0 or 1 as vectors are interpreted as unsigned
  signum (Word a) = Word $ vec (orG xs : repeat 0)
    where xs = velems a
  fromInteger i = Word $ sized (\n -> Vec (i `ofWidth` n))

ofWidth :: Integral a => a -> Int -> [Bit]
n `ofWidth` s = map boolToBit (intToSizedBin n s)