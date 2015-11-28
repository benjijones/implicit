{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}

module Implicit.Word where

import Implicit.PolyTree

import Prelude hiding (Word, foldr, take, drop, splitAt)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Prelude
import Lava.Arithmetic
import Lava.Generic

import Data.List(find, genericReplicate)
 
type Word w = PolyTree (Vec w Bit)

toList :: Word w -> [Vec w Bit]
toList = foldr (\a -> [a]) (:) (\n a rest -> genericReplicate n a ++ rest)

-- I want to eliminate the Atom type and translate 'Expr'
-- directly to 'Word' and functions between 'Word'
-- and then turn 'Word' into function between Bits
-- this may not be realized for a while, because it means you can't
-- run arbitrary expressions, only ones that you have statically
-- prepared your hardware to evaluatate

--toBits :: Word (S l) w -> [Bit]
--toBits = foldr velems (\head rest -> (velems head )
{-}
-- | Convert a Word to a list of its integer outputs
wordToInts :: Integral a => Word (S l) w -> [a]
wordToInts = map binToNat . map bitToBools . toBits
-}
fromBit :: Bit -> Word N1
fromBit = Single . vsingle

-- | This is only for using haskell's
-- integer literals to create Single instances
instance (N w) => Num (Word w) where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  abs = error "Words do not have a signed interpretation"
  -- just 0 or 1
  signum a = undefined
  fromInteger = Single . vrepeat . fromInteger

instance Eq (Word n) where
  (Single a) == (Single b) = bitToBool (a === b)
  (Cons ha ta) == (Cons hb tb) = (bitToBool $ ha === hb) && (ta == tb)
  (Many na ha ta) == (Many nb hb tb) = (na == nb) && (bitToBool $ ha === hb) && (ta == tb)

{-}
append :: (Add l1 l2 l3) => Word l1 w -> Word l2 w -> Word l3 w
append (Word a) (Word b) = Word $ a <++> b
-}
encodeInteger :: (N w) => Integer -> Word w
encodeInteger = fromInteger

decodeInteger :: Word w -> Integer
decodeInteger (Single a) = binToInt . map bitToBool . velems $ a
{-}
encodeVector :: (Mul l1 l2 l3) => (a -> Word l2 w) -> Vec l1 a -> Word l3 w
encodeVector f = Word . vconcat . vmap (unWord . f)

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))
-}

-- | convert a non-empty list of Singles
-- into a Word list with Cons
collapse :: [Word w] -> Word w
collapse [Single w] = Single w
collapse (Single w : rest) = Cons w (collapse rest)


{-}
wordMap :: (Word w -> a) -> Word l w -> Vec l a
wordMap f = vmap (f . Word . vsingle) . unWord

-- | Like 'unwords' from Prelude
separate :: Word l w -> Vec l (Word N1 w)
separate = vmap (Word . vsingle) . unWord
-}

-- | slices by length
-- starting at 'n'
-- and ending at 'm'
slice :: Integer -> Integer -> Word w -> Word w
slice n m = take (m - n) . drop n

-- | take lengthwise
take :: Integer -> Word w -> Word w
take 0 _ = error "Word length must be greater than 0" 
take 1 (Single word) = Single word
take n (Cons head rest) = Cons head (take (n - 1) rest)
take n (Many m head rest) = Many m head (take (n - 1) rest)

-- | drop lengthwise
drop :: Integer -> Word w -> Word w
drop 0 word = word
drop n (Cons _ rest) = drop (n - 1) rest
drop n (Many _ _ rest) = drop (n - 1) rest

-- | split widthwise
splitAt :: (N n, Add n m w) => n -> Word w -> (Word n, Word m)
splitAt n (Single word) = let (l, r) = vsplitAt n word in (Single l, Single r)
splitAt n (Cons head rest) = let (l, r) = vsplitAt n head in
                             let (restL, restR) = splitAt n rest in
                             (Cons l (restL), Cons r (restR))
splitAt n (Many m head rest) = let (l, r) = vsplitAt n head in
                               let (restL, restR) = splitAt n rest in
                               (Many m l restL, Many m r restR)

-- | combine widthwise
combine :: (Add w1 w2 w3) => Word w1 -> Word w2 -> Word w3
combine (Single l) (Single r) = Single $ l <++> r
{-}
at :: (N n, Less n l) => n -> Word l w -> Word N1 w
at n (Word w) = Word . vsingle $ w `vat` n

empty :: (N l, N w) => Word l w
empty = 0

-}