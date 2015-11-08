{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

getBits :: Word (S l) w -> [Bit]
getBits = velems . vhead . unWord

-- | Convert a Word to a list of its integer outputs
wordToInts :: Integral a => Word (S l) w -> [a]
wordToInts = map binToNat . map bitToBools . getBits

instance Show (Word d n) where
  show = show . unWord

-- this num instance only compiles when
-- the length of the word equals 1
-- and it's only for using haskell's
-- integer literals 
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

class Encode l w a where
  encode :: a -> Word l w
  decode :: Word l w -> a

instance Encode l w (Word l w) where
  encode = id
  decode = id

instance (N w) => Encode N1 w Integer where
  encode = fromInteger
  decode = binToInt . map bitToBool . velems . vhead . unWord

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))

combine :: (Add w1 w2 w3) => Word l w1 -> Word l w2 -> Word l w3
combine (Word l) (Word r) = Word (vzipWith (<++>) l r)

flatMap :: (Mul l2 l1 l3) => (a -> Word l1 w) -> Vec l2 a -> Word l3 w 
flatMap f = Word . vconcat . vmap (unWord . f)

wordMap :: (Word N1 w -> a) -> Word l w -> Vec l a
wordMap f = vmap (f . Word . vsingle) . unWord

splitAt :: (N n, Add n m w) => n -> Word l w -> (Word l n, Word l m)
splitAt n (Word w) = (Word $ vmap (vtake n) w, Word $ vmap (vdrop n) w)

matches :: (Encode l w a) => a -> Word l w -> Word N1 N1
matches a = Word . vsingle . vsingle . (=== encode a)