{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Implicit.Atom where
  
import Prelude hiding (Word, splitAt)
import Data.Bits

import Lava.Bit
import Lava.Word
import Lava.Vector
import Lava.Prelude

import Implicit.AtomType

data Atom a = Atom { atomDeleted :: Deleted
                   , atomType :: AtomType
                   , atomContents :: Integer }
  deriving Show

type Deleted = Bool

instance Encode N1 N1 Deleted where
  encode = Word . vsingle . vsingle . boolToBit
  decode = bitToBool . vhead . vhead . unWord

instance Encode N1 N11 (Atom N11) where
  encode atom = (encode (atomDeleted atom) :: Word N1 N1) `combine`
                (encode (atomType atom) :: Word N1 N4) `combine`
                (encode (atomContents atom) :: Word N1 N6)
  decode word = Atom (decode deleted :: Deleted)
                     (decode ty :: AtomType)
                     (decode contents :: Integer)
    where
      (deleted, rest) = splitAt n1 word
      (ty, contents) = splitAt n4 rest

instance Encode l N11 (Vec l (Atom N11)) where
  encode = flatMap (encode :: Atom N11 -> Word N1 N11)
  decode = wordMap (decode :: Word N1 N11 -> Atom N11)

typeBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d N4
typeBits = Word . vmap (vtake n4 . vdrop n1) . unWord

contentBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d n
contentBits = Word . vmap (vdrop n5) . unWord