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

-- this num instance only compiles when
-- the length of the word equals 1
-- and it just delegates to the underlying
-- Vec n Bit instance 
instance N n => Num (Word N1 n) where
  (Word a) + (Word b) = undefined
  (Word a) - (Word b) = undefined
  (Word a) * (Word b) = undefined
  abs = error "Words do not have a signed interpretation"
  -- just 0 or 1
  signum (Word a) = undefined
  fromInteger = Word . vsingle . fromInteger

instance Generic (Word d n) where
  generic (Word vec) = cons Word >< vec

instance Eq (Word N1 n) where
  a == b = bitToBool (a =/= b)

class Encode a where
  encode :: a -> Word l w
  decode :: Word l w -> a

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))

