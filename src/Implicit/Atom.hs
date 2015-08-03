module Implicit.Atom where

import Lava.Bit
import Lava.Vector


import Lava.Prelude

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
atomToVec UnArm        = low  +> high +> high +> 0
atomToVec (Let ref)    = high +> low  +> low  +> fromInteger ref -- 100(0|1)^n
atomToVec In           = high +> low  +> high +> 0               -- 101(0)^n
atomToVec (LetRef ref) = high +> high +> low  +> fromInteger ref -- 110(0|1)^n
atomToVec (UnLet ref)  = high +> high +> high +> fromInteger ref -- 111(0|1)^n

isData :: (N n) => Word (S (S (S (S n)))) -> Bit
isData w = let typeBits = vtail w in
               inv (typeBits `vat` n0) <&> inv (typeBits `vat` n1) <&> inv (typeBits `vat` n2)

isCase :: (N n) => Word (S (S (S (S n)))) -> Bit
isCase w = let typeBits = vtail w in
               inv (typeBits `vat` n0) <&> inv (typeBits `vat` n1) <&> typeBits `vat` n2

isUnArm :: (N n) => Word (S (S (S (S n)))) -> Bit
isUnArm w = let typeBits = vtail w in
               inv (typeBits `vat` n0) <&> inv (typeBits `vat` n1) <&> (typeBits `vat` n2)

isLet :: (N n) => Word (S (S (S (S n)))) -> Bit
isLet w = let typeBits = vtail w in
              (typeBits `vat` n0) <&> inv (typeBits `vat` n1) <&> inv (typeBits `vat` n2)

markDelete :: (N n) => Word (S (S (S (S n)))) -> Word (S (S (S (S n))))
markDelete w = high +> vtail w