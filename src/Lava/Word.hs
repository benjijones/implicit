{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- | This is only for using haskell's
-- integer literals - if the Word
-- is wider than N1, the number is repeated
-- to create the right width
instance (N l, N w) => Num (Word l w) where
  (Word a) + (Word b) = undefined
  (Word a) - (Word b) = undefined
  (Word a) * (Word b) = undefined
  abs = error "Words do not have a signed interpretation"
  -- just 0 or 1
  signum (Word a) = undefined
  fromInteger = Word . vrepeat . fromInteger

instance Generic (Word d n) where
  generic (Word vec) = cons Word >< vec

instance Eq (Word l n) where
  a == b = bitToBool (a === b)

encodeInteger :: (N w) => Integer -> Word N1 w
encodeInteger = fromInteger

decodeInteger :: (N w) => Word N1 w -> Integer
decodeInteger = binToInt . map bitToBool . velems . vhead . unWord

encodeVector :: (Mul l1 l2 l3) => (a -> Word l2 w) -> Vec l1 a -> Word l3 w
encodeVector f = Word . vconcat . vmap (unWord . f)

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))

combine :: (Add w1 w2 w3) => Word l w1 -> Word l w2 -> Word l w3
combine l r = Word (vzipWith (<++>) (unWord $ l) (unWord $ r))

wordMap :: (Word N1 w -> a) -> Word l w -> Vec l a
wordMap f = vmap (f . Word . vsingle) . unWord

splitAt :: (N n, Add n m w) => n -> Word l w -> (Word l n, Word l m)
splitAt n (Word w) = (Word $ vmap (vtake n) w, Word $ vmap (vdrop n) w)