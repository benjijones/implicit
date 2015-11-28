module Implicit.Atom where
  
import Prelude hiding (Word, splitAt)
import Data.Bits

import Lava.Bit
import Lava.Vector
import Lava.Prelude

import Implicit.AtomType
import Implicit.Word
import Implicit.PolyTree

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

encodeDeleted :: Deleted -> Word N1
encodeDeleted = Single . vsingle . boolToBit

decodeDeleted :: Word N1 -> Deleted
decodeDeleted (Single vec) = bitToBool . vhead $ vec

encodeAtom :: Atom -> Word AtomN
encodeAtom (Atom deleted ty contents) =
             (encodeDeleted deleted) `combine`
             (encodeAtomType ty) `combine`
             (encodeInteger contents :: Word DataN)

encodeAtoms :: [Atom] -> Word AtomN
encodeAtoms = collapse . map encodeAtom

decodeAtom :: Vec AtomN Bit -> Atom
decodeAtom atom = Atom (decodeDeleted (Single deleted))
                       (decodeAtomType (Single ty))
                       (decodeInteger (Single contents))
  where (deleted, rest) = vsplitAt n1 atom
        (ty, contents) = vsplitAt n4 rest

decodeAtoms :: Word AtomN -> [Atom]
decodeAtoms = map decodeAtom . toList
   

typeBits :: (N n) => Word (S (S (S (S (S n))))) -> Word N4
typeBits = fst . splitAt n4 . snd . splitAt n1

contentBits :: (N n) => Word (S (S (S (S (S n))))) -> Word n
contentBits = snd . splitAt n5