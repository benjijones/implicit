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

type Deleted = Bool

type AtomN = N10
type DataN = N5

instance Show Atom where
  show atom = (if atomDeleted atom then "X" else "O") ++ " " ++ show (atomType atom) ++ " " ++ show (atomContents atom)


showAtoms :: Vec l Atom -> String
showAtoms = unlines . map show . velems

encodeDeleted :: Deleted -> Word N1 N1
encodeDeleted = Word . vsingle . vsingle . boolToBit

decodeDeleted :: Word N1 N1 -> Deleted
decodeDeleted = bitToBool . vhead . vhead . unWord

encodeAtom :: Atom -> Word N1 AtomN
encodeAtom (Atom deleted ty contents) =
             (encodeDeleted deleted) `combine`
             (encodeAtomType ty) `combine`
             (encodeInteger contents :: Word N1 DataN)

decodeAtom :: Word N1 AtomN -> Atom
decodeAtom atom = Atom (decodeDeleted deleted)
                       (decodeAtomType ty)
                       (decodeInteger contents)
    where
      (deleted, rest) = splitAt n1 atom
      (ty, contents) = splitAt n4 rest

decodeAtoms :: Word l AtomN -> Vec l Atom
decodeAtoms = vmap decodeAtom . separate

typeBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d N4
typeBits = Word . vmap (vtake n4 . vdrop n1) . unWord

contentBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d n
contentBits = Word . vmap (vdrop n5) . unWord