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

data Atom = Atom { atomDeleted :: Deleted
                   , atomType :: AtomType
                   , atomContents :: Integer }
  deriving Show

type Deleted = Bool

type AtomN = N10
type DataN = N5
type TypeN = N4

encodeDeleted :: Deleted -> Word N1 N1
encodeDeleted = Word . vsingle . vsingle . boolToBit

decodeDeleted :: Word N1 N1 -> Deleted
decodeDeleted = bitToBool . vhead . vhead . unWord

encodeAtom :: Atom -> Word N1 AtomN
encodeAtom (Atom deleted ty contents) =
             (encodeDeleted deleted) `combine`
             (encodeAtomType ty :: Word N1 TypeN) `combine`
             (encodeInteger contents :: Word N1 DataN)

decodeAtom :: Word N1 AtomN -> Atom
decodeAtom atom = Atom (decodeDeleted deleted)
                       (decodeAtomType ty)
                       (decodeInteger contents)
    where
      (deleted, rest) = splitAt n1 atom
      (ty, contents) = splitAt n4 rest

typeBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d N4
typeBits = Word . vmap (vtake n4 . vdrop n1) . unWord

contentBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d n
contentBits = Word . vmap (vdrop n5) . unWord