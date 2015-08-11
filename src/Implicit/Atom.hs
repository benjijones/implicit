module Implicit.Atom where

import Lava.Bit
import Lava.Vector
import Lava.Prelude

import Data.Bits

data Atom a =
    Data Integer Delete

  -- branching
  | Case Delete
  | Arm (Integer) Delete
  | UnArm Delete

  -- let expressions
  | Let (Integer) Delete
  | In Delete
  | LetRef (Integer) Delete
  | UnLet (Integer) Delete

  deriving Show

type Delete = Bool

atomToInteger :: Atom a -> Integer
atomToInteger atom = 0 .|. (typeEncoding atom `shiftL` 1) .|. (atomContents atom `shiftL` 4)

atomContents :: Atom a -> Integer
atomContents (Data val _)   = val
atomContents (Case _)       = 0
atomContents (Arm arm _)    = arm
atomContents (UnArm  _)     = 0
atomContents (Let ref _)    = ref
atomContents (In _)         = 0
atomContents (LetRef ref _) = ref
atomContents (UnLet ref _)  = ref

typeEncoding :: Atom a -> Integer
typeEncoding (Data val _)   = 0
typeEncoding (Case _)       = 1
typeEncoding (Arm arm _)    = 2
typeEncoding (UnArm _)      = 3
typeEncoding (Let ref _)    = 4
typeEncoding (In _)         = 5
typeEncoding (LetRef ref _) = 6
typeEncoding (UnLet ref _)  = 7

typeBits :: (N n) => Word (S (S (S (S n)))) -> Word N3
typeBits = vtake n3 . vdrop n1

contentBits :: (N n) => Word (S (S (S (S n)))) -> Word n
contentBits = vdrop n4

wordToAtom :: (N n) => Word (S (S (S (S n)))) -> Atom n
wordToAtom w
  | bitToBool (isData w) = Data (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isCase w) = Case (isDeleted w)
  | bitToBool (isUnArm w) = UnArm (isDeleted w)
  | bitToBool (isLet w) = Let (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isIn w) = In (isDeleted w)
  | bitToBool (isLetRef w) = LetRef (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isUnLet w) = UnLet (wordToInt . contentBits $ w) (isDeleted w)

isData :: (N n) => Word (S (S (S (S n)))) -> Bit
isData = (=== 0) . typeBits

isCase :: (N n) => Word (S (S (S (S n)))) -> Bit
isCase = (=== 1) . typeBits

isUnArm :: (N n) => Word (S (S (S (S n)))) -> Bit
isUnArm = (=== 3) . typeBits

isLet :: (N n) => Word (S (S (S (S n)))) -> Bit
isLet = (=== 4) . typeBits

isIn :: (N n) => Word (S (S (S (S n)))) -> Bit
isIn = (=== 5) . typeBits

isLetRef :: (N n) => Word (S (S (S (S n)))) -> Bit
isLetRef = (=== 6) . typeBits

isUnLet :: (N n) => Word (S (S (S (S n)))) -> Bit
isUnLet = (=== 7) . typeBits

isDeleted :: Word (S n) -> Bool
isDeleted = bitToBool . vhead

markDelete :: (N n) => Word (S (S (S (S n)))) -> Word (S (S (S (S n))))
markDelete w = high +> vtail w