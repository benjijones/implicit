{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Lava.Word where

import Prelude hiding (Word)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Prelude
import Lava.Arithmetic
import Lava.Generic

import Data.List(find)

-- Contains a Bit signal with
-- type-level encodings of:
--  the width of the signal
-- the length of the signal
newtype Word length width = Word { unWord :: Vec length (Vec width Bit) }

wordMap :: (Word (S Z) w -> a) -> Word l w -> Vec l a
wordMap f = vmap (f . Word . vsingle) . unWord


getBits :: Word (S l) w -> [Bit]
getBits = velems . vhead . unWord

-- | Convert a Word to a list of its integer outputs
wordToInts :: Integral a => Word (S l) w -> [a]
wordToInts = map binToNat . map bitToBools . getBits

instance Show (Word d n) where
  show = show . unWord

{-}
instance N n => Num (Word d n) where
  (Word a) + (Word b) = Word $ vec (velems a /+/ velems b)
  (Word a) - (Word b) = Word $ vec (velems a /-/ velems b)
  (Word a) * (Word b) = error "Multiplication of Words is not yet supported"
  abs (Word a) = Word $ a
  -- just 0 or 1 as vectors are interpreted as unsigned
  signum (Word a) = Word $ vec (orG xs : repeat 0)
    where xs = velems a
  fromInteger i = Word $ sized (\n -> Vec (i `ofWidth` n))
-}
instance Generic (Word d n) where
  generic (Word vec) = cons Word >< vec

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))

