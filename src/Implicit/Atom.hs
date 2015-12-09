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

encodeAtom :: Atom -> Word DataN
encodeAtom (Atom deleted ty contents) =
            Word {
              deleted = boolToBit deleted
            , pattern = A ty
            , clockCycle = 0
            , typeBits = encodeAtomType ty
            , contents = fromInteger contents }

encodeAtoms :: [Atom] -> Sentence DataN
encodeAtoms = Prelude.foldr (\atom rest -> (encodeAtom atom `Cons` rest)) Nil

atomsToWord :: [Atom] -> Word DataN
atomsToWord = Prelude.foldr (\atom prev ->
  Word {
    deleted = Lava.Prelude.delay (boolToBit $ atomDeleted atom) $ deleted prev
  , pattern = pattern prev
  , clockCycle = clockCycle prev - 1
  , typeBits = Lava.Prelude.delay (encodeAtomType $ atomType atom) $ typeBits prev
  , contents = Lava.Prelude.delay (fromInteger $ atomContents atom) $ contents prev })
  (newWord 0 0)

decodeAtom :: Word DataN -> Atom
decodeAtom word = Atom (bitToBool $ deleted word)
                       (decodeAtomType $ typeBits word)
                       (binToNat . velems . vmap bitToBool $ contents word)

decodeAtoms :: Sentence DataN -> [Atom]
decodeAtoms = map decodeAtom . toList

wordToAtoms :: Word DataN -> [Atom]
wordToAtoms = unfoldr (\word ->
  if clockCycle word > 0
  then Nothing
  else Just (decodeAtom word, Word {
    deleted =  }))