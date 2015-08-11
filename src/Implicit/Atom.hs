module Implicit.Atom where

import Lava.Bit
import Lava.Vector
import Lava.Prelude

import Data.Bits

data Atom n =
    Data (Integer)

  -- branching
  | Case
  | Arm (Integer)
  | UnArm

  -- let expressions
  | Let (Integer)
  | In
  | LetRef (Integer)
  | UnLet (Integer)

  deriving Show

atomToInteger :: Atom n -> Integer
atomToInteger atom = 0 .|. (typeEncoding atom `shiftL` 1) .|. (atomContents atom `shiftL` 4)

atomContents :: Atom n -> Integer
atomContents (Data val)   = val
atomContents Case         = 0
atomContents (Arm arm)    = arm
atomContents UnArm        = 0
atomContents (Let ref)    = ref
atomContents In           = 0
atomContents (LetRef ref) = ref
atomContents (UnLet ref)  = ref

typeEncoding :: Atom n -> Integer
typeEncoding (Data val)   = 0
typeEncoding Case         = 1
typeEncoding (Arm arm)    = 2
typeEncoding UnArm        = 3
typeEncoding (Let ref)    = 4
typeEncoding In           = 5
typeEncoding (LetRef ref) = 6
typeEncoding (UnLet ref)  = 7

typeBits :: (N n) => Word (S (S (S (S n)))) -> Word N3
typeBits = vtake n3 . vdrop n1

contentBits :: (N n) => Word (S (S (S (S n)))) -> Word n
contentBits = vdrop n4

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

markDelete :: (N n) => Word (S (S (S (S n)))) -> Word (S (S (S (S n))))
markDelete w = vinit w <+ high