module Implicit.Atom where
  
import Prelude hiding (Word, splitAt)
import Data.Bits

import Lava.Bit
import Lava.Vector
import Lava.Prelude
import Lava.Binary

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

encodeAtom :: Atom -> Word DataN
encodeAtom (Atom deleted ty contents) =
            Word {
              deleted = boolToBit deleted
            , pattern = A ty
            , delays = 0
            , typeBits = encodeAtomType ty
            , contents = fromInteger contents }

encodeAtoms :: [Atom] -> Sentence DataN
encodeAtoms = Prelude.foldr (\atom rest -> (encodeAtom atom `Cons` rest)) Nil

decodeAtom :: Word DataN -> Atom
decodeAtom word = Atom (bitToBool $ deleted word)
                       (decodeAtomType $ typeBits word)
                       (binToNat . velems . vmap bitToBool $ contents word)

decodeAtoms :: Sentence DataN -> [Atom]
decodeAtoms = map decodeAtom . toList