{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}

module Implicit.Word where

import Implicit.AtomType
import Implicit.PolyTree

import Prelude hiding (Word, foldr, take, drop, splitAt)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Prelude
import Lava.Arithmetic
import Lava.Generic

import Data.List(find, genericReplicate)
 
data Word w = Word {
                deleted :: Bit
              , pattern :: Pattern
              , delays :: Integer
              , typeBits :: Vec N4 Bit
              , contents :: Vec w Bit }

data Pattern where
  A     :: AtomType -> Pattern
  Any   :: Pattern

-- | create a Word from scratch
newWord :: Vec N4 Bit -> Vec w Bit -> Word w
newWord = Word 0 Any 0  

type Sentence w = PolyTree (Word w)

-- | Overwrites the pattern stored by
-- the previous Word
matchPattern :: Pattern -> Word w -> Word w
matchPattern (A ty) prev  = Word {
                              deleted = (deleted prev) <|> (encodeAtomType ty === typeBits prev)
                            , pattern = A ty
                            , delays = delays prev
                            , typeBits = typeBits prev
                            , contents = contents prev }
matchPattern Any prev = Word {
                            deleted = deleted prev
                          , pattern = Any
                          , delays = delays prev
                          , typeBits = typeBits prev
                          , contents = contents prev }

match :: PolyTree Pattern -> Word w -> Sentence w
match patterns input = fmap (\(index, pattern) -> matchPattern pattern (withDelay index input)) (polyZipWith (,) [0..] patterns)


withDelay :: Integer -> Word w -> Word w
withDelay delay prev = Word {
                         deleted = deleted prev
                       , pattern = pattern prev
                       , delays = delay
                       , typeBits = typeBits prev
                       , contents = contents prev }

toList :: Sentence w -> [Word w]
toList = foldr [] (:) (:)

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
{-fromBit :: Bit -> Word N1
fromBit = Single . vsingle
-}
{-}
append :: (Add l1 l2 l3) => Word l1 w -> Word l2 w -> Word l3 w
append (Word a) (Word b) = Word $ a <++> b
-}
encodeInteger :: (N w) => Integer -> Word w
encodeInteger n = Word {
                         deleted = 0
                       , pattern = A Data
                       , delays = 0
                       , typeBits = encodeAtomType Data
                       , contents = fromInteger n }

decodeInteger :: Word w -> Integer
decodeInteger = binToInt . map bitToBool . velems . contents
{-}
encodeVector :: (Mul l1 l2 l3) => (a -> Word l2 w) -> Vec l1 a -> Word l3 w
encodeVector f = Word . vconcat . vmap (unWord . f)

delayW :: Word (S l) w -> Word (S (S l)) w
delayW (Word word) = Word (word <+ vmap (delay 0) (vlast word))


-- | convert a non-empty list of Singles
-- into a Word list with Cons
collapse :: [Word w] -> Word w
collapse [Single w] = Single w
collapse (Single w : rest) = Cons w (collapse rest)
-}

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
slice :: Integer -> Integer -> Sentence w -> Sentence w
slice n m = take (m - n) . drop n

-- | take lengthwise
take :: Integer -> Sentence w -> Sentence w
take 0 Nil = error "Word length must be greater than 0"
take n (Cons head rest) = Cons head (take (n - 1) rest)
take n (Many head rest) = Many head (take (n - 1) rest)

-- | drop lengthwise
drop :: Integer -> Sentence w -> Sentence w
drop 0 word = word
drop n (Cons _ rest) = drop (n - 1) rest
drop n (Many _ rest) = drop (n - 1) rest

{-}
at :: (N n, Less n l) => n -> Word l w -> Word N1 w
at n (Word w) = Word . vsingle $ w `vat` n
-}