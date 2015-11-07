module Implicit.Atom where
  
import Prelude hiding (Word)
import Data.Bits

import Lava.Bit
import Lava.Word
import Lava.Vector
import Lava.Prelude

import Implicit.AtomType

data Atom a = Atom { atomType :: AtomType
                   , atomDeleted :: Deleted
                   , atomContents :: Integer }
  deriving Show

type Deleted = Bool

atomsToWord :: (N w) => Vec l (Atom w) -> Word l w
atomsToWord = Word . vmap (fromInteger . atomToInteger)

atomToInteger :: Atom a -> Integer
atomToInteger atom = 0 .|. (typeEncoding atom `shiftL` 1) .|. (atomContents atom `shiftL` 5)

typeBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d N4
typeBits = Word . vmap (vtake n4 . vdrop n1) . unWord

contentBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d n
contentBits = Word . vmap (vdrop n5) . unWord

wordToAtoms :: (N w) => Word l (S (S (S (S (S w))))) -> Vec l (Atom (S (S (S (S (S w))))))
wordToAtoms = wordMap wordToAtom
  where
    wordToAtom w = Atom (decode w) (isDeleted) contentBits

typeEncoding :: Atom a -> Integer
typeEncoding (Data _ _)   = 0
typeEncoding (Case _ _)   = 1
typeEncoding (Arm _ _)    = 2
typeEncoding (Arrow _ _)  = 3
typeEncoding (UnCase _ _) = 4
typeEncoding (Let _ _)    = 5
typeEncoding (In _)       = 6
typeEncoding (LetRef _ _) = 7
typeEncoding (LetRefPadding _) = 8
typeEncoding (UnLet _ _)  = 9
typeEncoding (Add _)      = 10

isData, isCase, isArm, isArrow, isUnCase, isLet, isIn, isLetRef, isLetRefPadding, isUnLet, isAdd :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isData = (=== 0) . unWord . typeBits
isCase = (=== 1) . unWord . typeBits
isArm = (=== 2) . unWord . typeBits
isArrow = (=== 3) . unWord . typeBits
isUnCase = (=== 4) . unWord . typeBits
isLet = (=== 5) . unWord . typeBits
isIn = (=== 6) . unWord . typeBits
isLetRef = (=== 7) . unWord . typeBits
isLetRefPadding = (=== 8) . unWord . typeBits
isUnLet = (=== 9) . unWord . typeBits
isAdd = (=== 10) . unWord . typeBits

isDeleted :: Word d (S n) -> Bool
isDeleted = bitToBool . vhead . unWord

markDelete :: (N n) => Word d (S n) -> Word d (S n)
markDelete (Word w) = Word $ high +> vtail w