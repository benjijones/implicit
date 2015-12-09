{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}

module Implicit.Word where

import Implicit.AtomType
import Implicit.Context
import Implicit.PolyTree

import Prelude hiding (Word, take, drop, splitAt, zipWith)

import Lava.Bit
import Lava.Binary
import Lava.Vector
import Lava.Prelude
import Lava.Arithmetic
import qualified Lava.Generic as G

import Data.List(find, genericReplicate)
 
data Word w = Word {
                deleted :: Bit
              , pattern :: Pattern
              , clockCycle :: Integer
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
                            , clockCycle = clockCycle prev
                            , typeBits = typeBits prev
                            , contents = contents prev }
matchPattern Any prev = Word {
                            deleted = deleted prev
                          , pattern = Any
                          , clockCycle = clockCycle prev
                          , typeBits = typeBits prev
                          , contents = contents prev }

match :: PolyTree (Pattern, Word w -> Context (String, Word w) (Word w)) -> Sentence w -> Sentence w
match cases inputs = fst . (flip runContext) [] . sequence $ zipWith (\(pattern, f) -> f . matchPattern pattern) cases inputs

{- functions that modify Words- these should be used
   in place of manually modifying values in Word -}

delay :: (N w) => Word w -> Word w
delay word = Word {
                       deleted = G.delay 0 $ deleted word
                     , pattern = pattern word
                     , clockCycle = -1 + clockCycle word
                     , typeBits = G.delay 0 $ typeBits word
                     , contents = G.delay 0 $ contents word }

mapContents :: (Vec w Bit -> Vec w Bit) -> Word w -> Word w
mapContents f word = Word {
                        deleted = deleted word
                      , pattern = pattern word
                      , clockCycle = clockCycle word
                      , typeBits = typeBits word
                      , contents = f $ contents word  }

toList :: Sentence w -> [Word w]
toList = foldr (:) []

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
                       , clockCycle = 0
                       , typeBits = encodeAtomType Data
                       , contents = fromInteger n }

decodeInteger :: Word w -> Integer
decodeInteger = binToInt . map bitToBool . velems . contents
{-}

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