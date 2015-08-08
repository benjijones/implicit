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

atomToVec :: (N n) => Atom n -> Word (S (S (S n)))
atomToVec (Data val)   = low  +> low  +> low  +> fromInteger val -- 000(0|1)^n
atomToVec Case         = low  +> low  +> high +> 0               -- 001(0)^n
atomToVec (Arm arm)    = low  +> high +> low  +> fromInteger arm -- 010(0|1)^n
atomToVec UnArm        = low  +> high +> high +> 0               -- 011(0)^n
atomToVec (Let ref)    = high +> low  +> low  +> fromInteger ref -- 100(0|1)^n
atomToVec In           = high +> low  +> high +> 0               -- 101(0)^n
atomToVec (LetRef ref) = high +> high +> low  +> fromInteger ref -- 110(0|1)^n
atomToVec (UnLet ref)  = high +> high +> high +> fromInteger ref -- 111(0|1)^n

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

isData :: (N n) => Word (S (S (S (S n)))) -> Bit
isData w = typeBits w === 0

isCase :: (N n) => Word (S (S (S (S n)))) -> Bit
isCase w = typeBits w === 1

isUnArm :: (N n) => Word (S (S (S (S n)))) -> Bit
isUnArm w = typeBits w === 2

isLet :: (N n) => Word (S (S (S (S n)))) -> Bit
isLet w = typeBits w === 4

isUnLet :: (N n) => Word (S (S (S (S n)))) -> Bit
isUnLet w = typeBits w === 7

markDelete :: (N n) => Word (S (S (S (S n)))) -> Word (S (S (S (S n))))
markDelete w = vinit w <+ high